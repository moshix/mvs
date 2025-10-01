# Federated Chat API Documentation

## Overview

Teh Federated Chat system is a PostgreSQL-based global chat implementation that allows multiple BBS instances to communicate with eachother through a shared database. Each BBS connects to a central PostgreSQL server where all chat messages are stored and syncronized across all participatng systems.

## Architecture

### System Componets

1. **PostgreSQL Database**: Central message storage and synchronization point
2. **BBS Instances**: Individual BBS systems that conect to teh shared database
3. **Chat Messages**: Timestamped messages with username prefixes indicating orgin BBS

### Data Flow

```
BBS Instance A ──┐
                 ├──► PostgreSQL Database ◄──┤
BBS Instance B ──┘                           ├── BBS Instance C
                                             └── BBS Instance D
```

## Database Schema

### Primary Table: `chat`

The federated chat system uses a PostgreSQL table with the following structure:

```sql
CREATE TABLE chat (
    id SERIAL PRIMARY KEY,
    username VARCHAR(255) NOT NULL,
    message TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    room_id VARCHAR(50) DEFAULT 'global'
);
```

#### Table Fields

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `id` | SERIAL | Auto-incrementing primary key | `12345` |
| `username` | VARCHAR(255) | BBS-prefixed username in format "BBSName:username" | `"Forum3270:moshix"` |
| `message` | TEXT | The actual chat message content | `"Hello everyone!"` |
| `created_at` | TIMESTAMP | Message timestamp (UTC) | `2025-01-15 14:30:25` |
| `room_id` | VARCHAR(50) | Chat room identifier (always 'global' for federated chat) | `"global"` |

## Configuration

### BBS Configuration (tsu.cnf)

Each BBS instance must be configured with PostgreSQL connection paramaters in the `tsu.cnf` file:

```ini
# PostgreSQL Global Chat Configuration
globalchat_db_address=your-postgres-server.com
globalchat_db_port=5432
globalchat_db_user=globalchat_user
globalchat_db_password=your_secure_password
```

#### Configuration Paramters

| Parameter | Description | Required | Example |
|-----------|-------------|----------|---------|
| `globalchat_db_address` | PostgreSQL server hostname or IP | Yes | `chat.example.com` |
| `globalchat_db_port` | PostgreSQL server port | Yes | `5432` |
| `globalchat_db_user` | Database username | Yes | `chatuser` |
| `globalchat_db_password` | Database password | Yes | `securepass123` |

Teh database name is hardcoded as `globalchat` in teh system.

## API Operations

### 1. Post Message

**Function**: `postGlobalChatMessage(message string)`

**SQL Query**:
```sql
INSERT INTO chat (username, message, created_at, room_id) 
VALUES ($1, $2, CURRENT_TIMESTAMP, 'global')
```

**Parameters**:
- `$1`: Username in format "BBSName:username" (e.g., "Forum3270:moshix")
- `$2`: Message content

**Example Implementation (Python)**:
```python
import psycopg2
from datetime import datetime

def post_message(bbs_name, username, message, db_config):
    """Post a message to the federated chat"""
    conn = psycopg2.connect(
        host=db_config['host'],
        port=db_config['port'],
        user=db_config['user'],
        password=db_config['password'],
        database='globalchat'
    )
    
    try:
        cursor = conn.cursor()
        full_username = f"{bbs_name}:{username}"
        
        cursor.execute(
            "INSERT INTO chat (username, message, created_at, room_id) VALUES (%s, %s, CURRENT_TIMESTAMP, 'global')",
            (full_username, message)
        )
        conn.commit()
        print(f"Message posted successfully by {full_username}")
        
    except Exception as e:
        print(f"Error posting message: {e}")
        conn.rollback()
    finally:
        conn.close()

# Usage example
db_config = {
    'host': 'your-postgres-server.com',
    'port': 5432,
    'user': 'globalchat_user',
    'password': 'your_password'
}

post_message("MyBBS", "john", "Hello from MyBBS!", db_config)
```

**Example Implementation (Bash)**:
```bash
#!/bin/bash

# Configuration
DB_HOST="your-postgres-server.com"
DB_PORT="5432"
DB_USER="globalchat_user"
DB_PASSWORD="your_password"
DB_NAME="globalchat"

# Function to post a message
post_message() {
    local bbs_name="$1"
    local username="$2"
    local message="$3"
    local full_username="${bbs_name}:${username}"
    
    PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -c \
        "INSERT INTO chat (username, message, created_at, room_id) VALUES ('$full_username', '$message', CURRENT_TIMESTAMP, 'global');"
    
    if [ $? -eq 0 ]; then
        echo "Message posted successfully by $full_username"
    else
        echo "Error posting message"
    fi
}

# Usage example
post_message "MyBBS" "john" "Hello from bash script!"
```

### 2. Retrieve Messages

**Function**: `getGlobalChatMessages(limit int)`

**SQL Query**:
```sql
SELECT username, message, created_at
FROM chat
WHERE room_id = 'global'
ORDER BY created_at DESC
LIMIT $1
```

**Parameters**:
- `$1`: Maximum number of messages to retrieve

**Returns**: Array of messages in reverse chronolgical order (newest first). Note that usernames are returned in full format (`Forum3270:moshix`) from teh database - truncation to display format (`Fo:moshix`) happens only in teh chat interface.

**Example Implementation (Python)**:
```python
def get_messages(limit, db_config):
    """Retrieve recent messages from federated chat"""
    conn = psycopg2.connect(
        host=db_config['host'],
        port=db_config['port'],
        user=db_config['user'],
        password=db_config['password'],
        database='globalchat'
    )
    
    try:
        cursor = conn.cursor()
        cursor.execute(
            "SELECT username, message, created_at FROM chat WHERE room_id = 'global' ORDER BY created_at DESC LIMIT %s",
            (limit,)
        )
        
        messages = []
        for row in cursor.fetchall():
            username, message, created_at = row
            messages.append({
                'username': username,
                'message': message,
                'timestamp': created_at.isoformat()
            })
        
        # Reverse to get chronological order (oldest first)
        messages.reverse()
        return messages
        
    except Exception as e:
        print(f"Error retrieving messages: {e}")
        return []
    finally:
        conn.close()

# Usage example
messages = get_messages(50, db_config)
for msg in messages:
    # Note: msg['username'] contains full format like "Forum3270:moshix"
    # For display, you might want to truncate it to "Fo:moshix"
    display_username = format_username_for_display(msg['username'])  # Optional
    print(f"[{msg['timestamp']}] {msg['username']}: {msg['message']}")
```

**Example Implementation (Bash)**:
```bash
#!/bin/bash

get_messages() {
    local limit="$1"
    
    PGPASSWORD="$DB_PASSWORD" psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -t -c \
        "SELECT username || ' | ' || message || ' | ' || created_at FROM chat WHERE room_id = 'global' ORDER BY created_at DESC LIMIT $limit;" | \
        tac  # Reverse order to get chronological
}

# Usage example
echo "Recent messages:"
get_messages 10
```

### 3. Get Statistics

**Function**: `getGlobalChatStats()`

**SQL Queries**:

1. **Last Message Time**:
```sql
SELECT MAX(created_at) 
FROM chat 
WHERE room_id = 'global' 
AND created_at > NOW() - INTERVAL '24 hours'
```

2. **Message Count (24h)**:
```sql
SELECT COUNT(*) 
FROM chat 
WHERE room_id = 'global' 
AND created_at > NOW() - INTERVAL '24 hours'
```

3. **Active Users (30m)**:
```sql
SELECT COUNT(DISTINCT username) 
FROM chat 
WHERE room_id = 'global' 
AND created_at > NOW() - INTERVAL '30 minutes'
```

**Example Implementation (Python)**:
```python
def get_chat_stats(db_config):
    """Get federated chat statistics"""
    conn = psycopg2.connect(
        host=db_config['host'],
        port=db_config['port'],
        user=db_config['user'],
        password=db_config['password'],
        database='globalchat'
    )
    
    try:
        cursor = conn.cursor()
        
        # Last message time
        cursor.execute("""
            SELECT MAX(created_at) 
            FROM chat 
            WHERE room_id = 'global' 
            AND created_at > NOW() - INTERVAL '24 hours'
        """)
        last_msg_time = cursor.fetchone()[0]
        
        # Message count in last 24h
        cursor.execute("""
            SELECT COUNT(*) 
            FROM chat 
            WHERE room_id = 'global' 
            AND created_at > NOW() - INTERVAL '24 hours'
        """)
        msg_count_24h = cursor.fetchone()[0]
        
        # Active users in last 30m
        cursor.execute("""
            SELECT COUNT(DISTINCT username) 
            FROM chat 
            WHERE room_id = 'global' 
            AND created_at > NOW() - INTERVAL '30 minutes'
        """)
        active_users_30m = cursor.fetchone()[0]
        
        return {
            'last_message_time': last_msg_time.isoformat() if last_msg_time else None,
            'messages_24h': msg_count_24h,
            'active_users_30m': active_users_30m
        }
        
    except Exception as e:
        print(f"Error getting stats: {e}")
        return None
    finally:
        conn.close()

# Usage example
stats = get_chat_stats(db_config)
if stats:
    print(f"Last message: {stats['last_message_time']}")
    print(f"Messages in last 24h: {stats['messages_24h']}")
    print(f"Active users in last 30m: {stats['active_users_30m']}")
```

## Username Format

### BBS Prefix Format

Usernames in the federated chat follow the format: `BBSName:username`

**Examples**:
- `Forum3270:moshix`
- `RetroNet:john_doe`
- `ClassicBBS:admin`

### Display Format

**Storage vs Display**: The database stores the full username format (`Forum3270:moshix`), but the chat interface displays a truncated version to fit screen constraits.

**Display Truncation Rules**:
- BBS name: Truncated to first 2 characters
- Username: Truncated to maximum 8 characters  
- Display format: `BB:username`

**Examples**:
- **Stored**: `Forum3270:moshix` → **Displayed**: `Fo:moshix`
- **Stored**: `RetroNet:john_doe` → **Displayed**: `Re:john_doe`
- **Stored**: `ClassicBBS:administrator` → **Displayed**: `Cl:administ`

**Important**: When reading from teh database, you get teh full username. When displaying in chat history, the system automatically truncates for screen formatting.

**Helper Function for Display Formatting**:
```python
def format_username_for_display(full_username):
    """Convert 'Forum3270:moshix' to 'Fo:moshix' for display"""
    parts = full_username.split(':')
    if len(parts) != 2:
        return full_username[:9]  # Fallback for invalid format
    
    bbs_name, username = parts
    # Truncate BBS name to 2 chars, username to 8 chars
    truncated_bbs = bbs_name[:2]
    truncated_user = username[:8]
    
    return f"{truncated_bbs}:{truncated_user}"
```

## Connection Management

### Connection Parameters

```go
// Connection string format
connStr := fmt.Sprintf("host=%s port=%s user=%s password=%s dbname=%s sslmode=disable",
    config.Address, config.Port, config.User, config.Password, "globalchat")
```

### Connection Settings

- **Connection Timeout**: 3 seconds
- **SSL Mode**: Disabled (`sslmode=disable`)
- **Database Name**: Fixed as `globalchat`

### Error Handling

Common connection errors and thier meanings:

| Error | Cause | Solution |
|-------|-------|----------|
| Connection timeout | Network issues or server down | Check network conectivity and server status |
| Authentication failed | Wrong credentials | Verify username/password in configuration |
| Database not found | Database doesn't exist | Create teh `globalchat` database |
| Permission denied | User lacks priviliges | Grant apropriate permissions to database user |

## Security Considerations

### Database Security

1. **User Permissions**: Create a dedicated database user with minimal requried permissions:
```sql
CREATE USER globalchat_user WITH PASSWORD 'secure_password';
GRANT SELECT, INSERT ON chat TO globalchat_user;
GRANT USAGE, SELECT ON SEQUENCE chat_id_seq TO globalchat_user;
```

2. **Network Security**: 
   - Use firewall rules to restrict database acess
   - Consider VPN for production deployements
   - Monitor connection atempts

### Input Validaton

- Message content should be sanitized to prevent SQL injection
- Username format should be validated
- Message lenght limits should be enforced

## Monitoring and Maintenance

### Performance Monitoring

Monitor these metrics for optmial performance:

- Connection count to PostgreSQL
- Query execution time
- Message insertion rate
- Database size groth

### Maintanence Tasks

1. **Regular Cleanup**:
```sql
-- Remove messages older than 30 days
DELETE FROM chat 
WHERE room_id = 'global' 
AND created_at < NOW() - INTERVAL '30 days';
```

2. **Index Optimization**:
```sql
-- Recommended indexes
CREATE INDEX idx_chat_room_created ON chat(room_id, created_at);
CREATE INDEX idx_chat_created ON chat(created_at);
```

## Troubleshooting

### Common Issues

1. **Messages not appearing**: Check database conectivity and permissions
2. **Duplicate messages**: Verify unique constraints and connection handeling
3. **Performance issues**: Review indexes and query optimiztion
4. **Connection failures**: Check network connectivity and firewall setings

### Debug Queries

```sql
-- Check recent activity
SELECT COUNT(*), MAX(created_at) FROM chat WHERE room_id = 'global';

-- View active BBSes
SELECT DISTINCT SPLIT_PART(username, ':', 1) as bbs_name, COUNT(*) 
FROM chat 
WHERE room_id = 'global' 
AND created_at > NOW() - INTERVAL '1 hour'
GROUP BY bbs_name;

-- Check message distribution
SELECT DATE(created_at) as date, COUNT(*) as message_count
FROM chat 
WHERE room_id = 'global'
GROUP BY DATE(created_at)
ORDER BY date DESC
LIMIT 7;
```

## Example Complete Implementation

### Python Client Library

```python
import psycopg2
import json
from datetime import datetime, timedelta

class FederatedChatClient:
    def __init__(self, bbs_name, db_config):
        self.bbs_name = bbs_name
        self.db_config = db_config
    
    def connect(self):
        return psycopg2.connect(
            host=self.db_config['host'],
            port=self.db_config['port'],
            user=self.db_config['user'],
            password=self.db_config['password'],
            database='globalchat'
        )
    
    def post_message(self, username, message):
        conn = self.connect()
        try:
            cursor = conn.cursor()
            full_username = f"{self.bbs_name}:{username}"
            cursor.execute(
                "INSERT INTO chat (username, message, created_at, room_id) VALUES (%s, %s, CURRENT_TIMESTAMP, 'global')",
                (full_username, message)
            )
            conn.commit()
            return True
        except Exception as e:
            print(f"Error posting message: {e}")
            conn.rollback()
            return False
        finally:
            conn.close()
    
    def get_messages(self, limit=50):
        conn = self.connect()
        try:
            cursor = conn.cursor()
            cursor.execute(
                "SELECT username, message, created_at FROM chat WHERE room_id = 'global' ORDER BY created_at DESC LIMIT %s",
                (limit,)
            )
            messages = []
            for row in cursor.fetchall():
                username, message, created_at = row
                messages.append({
                    'username': username,
                    'message': message,
                    'timestamp': created_at
                })
            messages.reverse()  # Chronological order
            return messages
        except Exception as e:
            print(f"Error retrieving messages: {e}")
            return []
        finally:
            conn.close()
    
    def get_stats(self):
        conn = self.connect()
        try:
            cursor = conn.cursor()
            
            # Get all stats in one transaction
            cursor.execute("""
                SELECT 
                    (SELECT MAX(created_at) FROM chat WHERE room_id = 'global' AND created_at > NOW() - INTERVAL '24 hours') as last_msg,
                    (SELECT COUNT(*) FROM chat WHERE room_id = 'global' AND created_at > NOW() - INTERVAL '24 hours') as msg_24h,
                    (SELECT COUNT(DISTINCT username) FROM chat WHERE room_id = 'global' AND created_at > NOW() - INTERVAL '30 minutes') as users_30m
            """)
            
            result = cursor.fetchone()
            return {
                'last_message_time': result[0],
                'messages_24h': result[1],
                'active_users_30m': result[2]
            }
        except Exception as e:
            print(f"Error getting stats: {e}")
            return None
        finally:
            conn.close()

# Usage example
if __name__ == "__main__":
    db_config = {
        'host': 'your-postgres-server.com',
        'port': 5432,
        'user': 'globalchat_user',
        'password': 'your_password'
    }
    
    client = FederatedChatClient("MyBBS", db_config)
    
    # Post a message
    client.post_message("testuser", "Hello from Python client!")
    
    # Get recent messages
    messages = client.get_messages(10)
    for msg in messages:
        print(f"[{msg['timestamp']}] {msg['username']}: {msg['message']}")
    
    # Get statistics
    stats = client.get_stats()
    if stats:
        print(f"Stats: {json.dumps(stats, default=str, indent=2)}")
```

This documentation provides a complete refrence for implementing federated chat clients in any programing language that supports PostgreSQL connectivity.

Moshix / Sept 30, 2025 - Palo Alto, CA  
