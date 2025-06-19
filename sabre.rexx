/* SABRE CRS DEMO IN BREXX FOR VM/370 CMS BY ERNIETECH - ENHANCED VERSION */
/* EXPANDED WITH DYNAMIC DATA AND MORE SABRE COMMANDS
    - JUNE2025 MOSHIX - AUSTRIA*/

/* Clear screen in a cross-platform way */
address command
'clear'
if rc \= 0 then 'cls'
address

say center('-------------- SABRE Computer Reservation System (CRS)-----------',78)

/* Display date and WAFL:CONE */
today    = date('S')                                        /* YYYYMMDD */
ddmmyyyy = substr(today,7,2)'.'substr(today,5,2)'.'substr(today,1,4)
say ddmmyyyy "WAFL:CONE"

/* --- Valid Airport Codes --- */
/* North America */
airports.ATL = 'Atlanta Hartsfield-Jackson'
airports.LAX = 'Los Angeles International'
airports.ORD = 'Chicago O''Hare'
airports.DFW = 'Dallas/Fort Worth'
airports.DEN = 'Denver International'
airports.JFK = 'New York Kennedy'
airports.SFO = 'San Francisco International'
airports.LAS = 'Las Vegas McCarran'
airports.SEA = 'Seattle-Tacoma'
airports.MCO = 'Orlando International'
airports.EWR = 'Newark Liberty'
airports.MIA = 'Miami International'
airports.PHX = 'Phoenix Sky Harbor'
airports.IAH = 'Houston Bush'
airports.BOS = 'Boston Logan'
airports.MSP = 'Minneapolis-St Paul'
airports.DTW = 'Detroit Metro'
airports.FLL = 'Fort Lauderdale'
airports.CLT = 'Charlotte Douglas'
airports.LGA = 'New York LaGuardia'
airports.BWI = 'Baltimore/Washington'
airports.SLC = 'Salt Lake City'
airports.YYZ = 'Toronto Pearson'
airports.YVR = 'Vancouver International'
airports.YUL = 'Montreal Trudeau'

/* Europe */
airports.LHR = 'London Heathrow'
airports.CDG = 'Paris Charles de Gaulle'
airports.AMS = 'Amsterdam Schiphol'
airports.FRA = 'Frankfurt International'
airports.IST = 'Istanbul International'
airports.MAD = 'Madrid Barajas'
airports.BCN = 'Barcelona El Prat'
airports.LGW = 'London Gatwick'
airports.MUC = 'Munich International'
airports.FCO = 'Rome Fiumicino'
airports.SVO = 'Moscow Sheremetyevo'
airports.DME = 'Moscow Domodedovo'
airports.DUB = 'Dublin International'
airports.ZRH = 'Zurich International'
airports.CPH = 'Copenhagen Kastrup'
airports.OSL = 'Oslo Gardermoen'
airports.ARN = 'Stockholm Arlanda'
airports.VIE = 'Vienna International'
airports.BRU = 'Brussels International'
airports.MXP = 'Milan Malpensa'

/* Asia Pacific */
airports.PEK = 'Beijing Capital'
airports.HND = 'Tokyo Haneda'
airports.DXB = 'Dubai International'
airports.HKG = 'Hong Kong International'
airports.ICN = 'Seoul Incheon'
airports.BKK = 'Bangkok Suvarnabhumi'
airports.SIN = 'Singapore Changi'
airports.CGK = 'Jakarta Soekarno-Hatta'
airports.KUL = 'Kuala Lumpur International'
airports.DEL = 'Delhi Indira Gandhi'
airports.BOM = 'Mumbai International'
airports.SYD = 'Sydney Kingsford Smith'
airports.MEL = 'Melbourne International'
airports.AKL = 'Auckland International'
airports.KIX = 'Osaka Kansai'
airports.TPE = 'Taipei Taoyuan'
airports.MNL = 'Manila Ninoy Aquino'
airports.CAN = 'Guangzhou Baiyun'
airports.PVG = 'Shanghai Pudong'
airports.NRT = 'Tokyo Narita'

/* Middle East & Africa */
airports.DOH = 'Doha Hamad'
airports.AUH = 'Abu Dhabi International'
airports.CAI = 'Cairo International'
airports.JNB = 'Johannesburg O.R. Tambo'
airports.CPT = 'Cape Town International'
airports.TLV = 'Tel Aviv Ben Gurion'
airports.BAH = 'Bahrain International'
airports.RUH = 'Riyadh King Khalid'
airports.JED = 'Jeddah King Abdulaziz'
airports.MCT = 'Muscat International'

/* Latin America */
airports.GRU = 'Sao Paulo Guarulhos'
airports.MEX = 'Mexico City International'
airports.BOG = 'Bogota El Dorado'
airports.LIM = 'Lima Jorge Chavez'
airports.SCL = 'Santiago International'
airports.GIG = 'Rio de Janeiro Galeao'
airports.EZE = 'Buenos Aires Ezeiza'
airports.PTY = 'Panama City Tocumen'
airports.CUN = 'Cancun International'
airports.UIO = 'Quito International'

/* --- Equipment Descriptions with Seat Configurations --- */
eqDesc.A320 = 'Airbus A320-100'
eqDesc.B738 = 'Boeing 737-800'
eqDesc.B789 = 'Boeing 787-9'
eqDesc.A350 = 'Airbus A350-100'
eqDesc.B77W = 'Boeing 777-300ER'
eqDesc.B747 = 'Boeing 747-800'
eqDesc.B767 = 'Boeing 767-300'
eqDesc.B757 = 'Boeing 757-200'
eqDesc.MD981 = 'MD-8-81'


/* Seat configurations per equipment type */
eqSeats.A320 = 158
eqSeats.B738 = 162
eqSeats.B789 = 290
eqSeats.A350 = 308
eqSeats.B77W = 357
eqSeats.B747 = 412
eqSeats.B767 = 203
eqSeats.B757 = 152
eqSeats.MD981 = 109

/* Airlines */
airlines.1 = 'AAL'  /* American Airlines */
airlines.2 = 'DAL'  /* Delta Airlines */
airlines.3 = 'LH'   /* Lufthansa */
airlines.0 = 3

/* Flight number ranges per airline */
flightNumRange.AAL = '100 999'
flightNumRange.DAL = '1000 1999'
flightNumRange.LH = '400 799'

/* --- Frequent Flyer Data --- */
ff.0 = 0                    /* Number of FF entries */
ffPartners.AAL = 'BA QF CX JL'    /* American Airlines partners */
ffPartners.DAL = 'AF KL VS KE'    /* Delta partners */
ffPartners.LH = 'AC OS SN LX'     /* Lufthansa group */

/* --- Queue Management --- */
queue.0 = 5                 /* Number of queues */
queueType.1 = 'GENERAL'     /* General queue */
queueType.2 = 'TICKETING'   /* Ticketing time limit */
queueType.3 = 'SCHEDULE'    /* Schedule changes */
queueType.4 = 'WAITLIST'    /* Waitlist processing */
queueType.5 = 'SPECIAL'     /* Special handling */

/* --- Advanced Pricing --- */
fareTypes.Y = 'FULL'        /* Full fare economy */
fareTypes.B = 'FLEX'        /* Flexible economy */
fareTypes.M = 'ECON'        /* Economy */
fareTypes.Q = 'DISC'        /* Discount economy */

/* --- Enhanced Name Fields --- */
paxTypes.ADT = 'ADULT'
paxTypes.CHD = 'CHILD'
paxTypes.INF = 'INFANT'
paxTypes.SNR = 'SENIOR'
paxTypes.STU = 'STUDENT'

/* --- Sign-In Loop --- */
do forever
    say
    say "* Enter Agent Sign-in (e.g. SIA*01762):"
    pull input
    input = strip(translate(input))    /* uppercase & trim */
    
    /* Validate format: must be SIA* followed by numbers */
    if left(input, 4) \= 'SIA*' then do
        say '*** Sign-in must start with SIA*'
        iterate
    end
    leave
end

/* --- Booking storage (not persistent) --- */
PNRdata.0 = 0               /* number of bookings */

/* --- Flight seat inventory (persisted per flight key) --- */
flightSeats. = 0
flightMaxSeats. = 0

/* --- Main Loop --- */
say
say "* SABRE DEMO: Type HELP for instructions"
say

/* Initialize flight seat tracking */
drop flightSeats.
flightSeats.0 = 0

do forever
    say
    pull cmd
    cmd = strip(translate(cmd))

    /* HELP & SIGN-OUT */
    if cmd = 'HELP' then do
        say "QUERY FORMAT: 18OCT JFK ZRH (optional: 9A)"
        say "BOOK <n>      Book flight Index number <n> (e.g. BOOK 1 or BOOK 3)"
        say "CANCEL <PNR>  Cancel booking (e.g. CANCEL 001 or CANCEL PNR001)"
        say "PNR <loc>     Display booking details (e.g. PNR 001 or PNR PNR001)"
        say "W/EQ*<code>   Filter flights by equipment code (e.g. A320 or B738)"
        say "FF<pnr>       Display FF info for PNR (e.g. FF001 or FFPNR001)"
        say "FF<pnr>/<num> Add FF number to PNR (e.g. FF001/AA123456)"
        say "FF<pnr>/*     Delete FF number from PNR"
        say "Q/C           Display queue counts"
        say "Q/P/<n>/<pnr> Place PNR in queue n"
        say "WP/NCB <n>    Price and book lowest available fare for segment n"
        say "WP/NI         Display alternate fare options"
        say "N/ADD-<n>     Add passenger name (N/ADD-1 DOE/JOHN ADT)"
        say "N/CHG-<n>     Change passenger name (N/CHG-1 DOE/JANE)"
        say "SO*           Sign Out all Work Areas"
        iterate
    end
    
    if cmd = 'SO*' then do
        say "Agent Sign Out complete"
        exit
    end

    /* split into verb + rest */
    parse var cmd first rest

    /* --- BOOK command with seat & name prompting --- */
    if first = 'BOOK' then do
        parse var rest num
        if \datatype(num,'W') | num < 1 | num > matchList.0 then do
            say "*** Invalid flight number"
            iterate
        end
        
        parse var matchList.num airline flight depc depd dept arrt arrc avst eqmt
        flightKey = airline||flight||depc||arrc||depd||dept
        
        /* Get current seat count */
        seats = flightSeats.flightKey
        if seats = '' then do
            seats = avst + 0  /* Force numeric conversion */
            flightSeats.flightKey = seats
        end
        
        seats = seats + 0  /* Force numeric conversion */
        
        if seats <= 0 then do
            say "*** Flight is full"
            iterate
        end

        /* generate new PNR */
        PNRdata.0 = PNRdata.0 + 1
        pnrnum = PNRdata.0
        pnr = 'PNR'||substr('000'||pnrnum,length('000'||pnrnum)-2,3)

        /* prompt for passenger name */
        say "* Enter passenger name as <first initial><last name> (no spaces):"
        pull pname

        /* assign next seat */
        seatCode = genRandomNum(1, 100)  /* Simple seat number */
        
        /* store booking */
        j = pnrnum
        PNRdata.j = pnr||' '||pname||' '||seatCode||' '||matchList.num

        /* decrement available seats */
        flightSeats.flightKey = seats - 1

        say 'BOOKED '||pnr||' PASSENGER '||pname||' SEAT '||seatCode
        iterate
    end

    /* --- Frequent Flyer Commands --- */
    if left(first, 2) = 'FF' then do
        /* Extract PNR from command */
        pnrRaw = substr(first, 3)
        parse var rest '/' ffnum

        /* Normalize PNR format */
        if left(pnrRaw, 3) \= 'PNR' then do
            padded = substr('000'||pnrRaw, length('000'||pnrRaw)-2, 3)
            pnr = 'PNR'||padded
        end
        else pnr = pnrRaw

        /* Find PNR in database */
        found = 0
        do j = 1 to PNRdata.0
            parse var PNRdata.j pnrEntry pname seat flightRec
            if pnrEntry = pnr then do
                found = 1
                if ffnum = '' then do
                    /* Display FF info */
                    if ff.pnr = '' then
                        say 'No FF number stored for' pnr
                    else
                        say 'FF:' ff.pnr '('ff.pnr.airline') for' pnr
                end
                else if ffnum = '*' then do
                    /* Delete FF number */
                    ff.pnr = ''
                    ff.pnr.airline = ''
                    say 'Frequent flyer number deleted for' pnr
                end
                else do
                    /* Add FF number */
                    ff.pnr = ffnum
                    ff.pnr.airline = left(ffnum, 2)
                    say 'Frequent flyer number' ffnum 'added for' pnr
                end
                leave
            end
        end
        if \found then say '*** PNR' pnr 'not found'
        iterate
    end

    /* --- Queue Management Commands --- */
    if left(first, 2) = 'Q/' then do
        parse var cmd 'Q/' qcmd rest
        select
            when qcmd = 'C' then do
                /* Display queue counts */
                say 'Queue Counts:'
                say '------------'
                do i = 1 to queue.0
                    /* Initialize queue count if not set */
                    if symbol('queue.'i) = 'LIT' then do
                        queue.i = 0
                        qcount = 0
                    end
                    else do
                        /* Force numeric conversion safely */
                        qcount = queue.i + 0
                        if \datatype(qcount, 'W') then qcount = 0
                    end
                    
                    say 'Queue' i '('||queueType.i||'): ' qcount 'PNRs'
                    if qcount > 0 then do
                        say '   PNRs:'
                        do j = 1 to qcount
                            if symbol('queue.'i'.'j) = 'LIT' then 
                                nop  /* Skip unset entries */
                            else
                                say '   -' queue.i.j
                        end
                    end
                end
            end
            when left(qcmd, 2) = 'P/' then do
                /* Place PNR in queue */
                parse var rest qnum '/' pnr
                if \datatype(qnum,'W') | qnum < 1 | qnum > queue.0 then do
                    say '*** Invalid queue number'
                    iterate
                end
                /* Normalize PNR format */
                if left(pnr,3) \= 'PNR' then do
                    padded = substr('000'||pnr,length('000'||pnr)-2,3)
                    pnr = 'PNR'||padded
                end
                /* Initialize queue count if needed */
                if symbol('queue.'qnum) = 'LIT' then queue.qnum = 0
                qcount = queue.qnum + 0  /* Force numeric conversion */
                if \datatype(qcount, 'W') then qcount = 0
                qcount = qcount + 1
                /* Store PNR in queue */
                queue.qnum.qcount = pnr
                queue.qnum = qcount
                say 'PNR' pnr 'placed in queue' qnum '('||queueType.qnum||')'
            end
            otherwise
                say '*** Invalid queue command'
        end
        iterate
    end

    /* --- Advanced Pricing Commands --- */
    if left(first, 3) = 'WP/' then do
        parse var cmd 'WP/' pcmd rest
        select
            when pcmd = 'NCB' then do
                /* Price and book lowest available fare */
                if \datatype(rest,'W') then do
                    say '*** Invalid segment number'
                    iterate
                end
                say 'Searching for lowest available fare...'
                do i = 1 to words(fareTypes)
                    fare = word(fareTypes, i)
                    say right(fare,1) 'class ('||fareTypes.fare||'): $'||format(genRandomNum(100,1000),3)||'.00'
                end
            end
            when pcmd = 'NI' then do
                /* Search for alternate fares */
                say 'Searching for alternative fares...'
                do i = 1 to words(fareTypes)
                    fare = word(fareTypes, i)
                    say right(fare,1) 'class ('||fareTypes.fare||'): $'||format(genRandomNum(100,1000),3)||'.00'
                end
            end
            otherwise
                say '*** Invalid pricing command. Use WP/NCB <segment> or WP/NI'
        end
        iterate
    end

    /* --- Enhanced Name Commands --- */
    if left(first, 2) = 'N/' then do
        parse var cmd 'N/' ncmd rest
        parse var ncmd cmd_type '-' paxnum
        select
            when cmd_type = 'ADD' then do
                /* Add new passenger name */
                if \datatype(paxnum,'W') then do
                    say '*** Invalid passenger number'
                    iterate
                end
                parse var rest lastname '/' firstname type
                if type = '' then type = 'ADT'
                if \wordpos(type, 'ADT CHD INF SNR STU') then do
                    say '*** Invalid passenger type'
                    iterate
                end
                /* Find PNR for this passenger number */
                if paxnum > PNRdata.0 then do
                    say '*** Invalid PNR number'
                    iterate
                end
                parse var PNRdata.paxnum pnrEntry oldname seat flightRec
                if pnrEntry = '' then do
                    say '*** No PNR found for passenger' paxnum
                    iterate
                end
                /* Update PNR record */
                newname = lastname||'/'||firstname
                PNRdata.paxnum = pnrEntry newname seat flightRec
                paxName.paxnum = newname
                paxType.paxnum = type
                say 'Added passenger:' newname 'Type:' paxTypes.type 'to' pnrEntry
            end
            when cmd_type = 'CHG' then do
                /* Change passenger name */
                if \datatype(paxnum,'W') then do
                    say '*** Invalid passenger number'
                    iterate
                end
                if paxnum > PNRdata.0 then do
                    say '*** Invalid PNR number'
                    iterate
                end
                parse var PNRdata.paxnum pnrEntry oldname seat flightRec
                if pnrEntry = '' then do
                    say '*** No PNR found for passenger' paxnum
                    iterate
                end
                parse var rest lastname '/' firstname
                /* Update PNR record */
                newname = lastname||'/'||firstname
                PNRdata.paxnum = pnrEntry newname seat flightRec
                paxName.paxnum = newname
                say 'Changed passenger name to:' newname 'in' pnrEntry
            end
            otherwise
                say '*** Invalid name command. Use N/ADD-<n> or N/CHG-<n>'
        end
        iterate
    end

    /* --- CANCEL command --- */
    if first = 'CANCEL' then do
        parse var rest pnrRaw
        pnrNorm = strip(translate(pnrRaw))
        if left(pnrNorm,3) \= 'PNR' then do
            padded = substr('000'||pnrNorm,length('000'||pnrNorm)-2,3)
            pnr = 'PNR'||padded
        end
        else pnr = pnrNorm

        found = 0
        do j = 1 to PNRdata.0
            parse var PNRdata.j pnrEntry restInfo
            if pnrEntry = pnr then do
                parse var PNRdata.j pnrEntry pname seat flightNum
                parse var matchList.flightNum airline flight depc depd dept arrt arrc avst eqmt
                flightKey = airline||flight||depc||arrc||depd||dept
                
                /* Handle seat inventory safely */
                if symbol('flightSeats.'flightKey) = 'LIT' then
                    flightSeats.flightKey = 0
                currSeats = flightSeats.flightKey + 0  /* Force numeric */
                if \datatype(currSeats, 'W') then currSeats = 0
                flightSeats.flightKey = currSeats + 1  /* Return seat to inventory */
                
                /* Mark PNR as cancelled */
                PNRdata.j = pnrEntry||' '||restInfo||' CANCELLED'
                say 'CANCELLED booking '||pnr
                found = 1
                leave
            end
        end
        if found = 0 then say '*** PNR '||pnr||' not found.'
        iterate
    end

    /* --- PNR lookup command --- */
    if first = 'PNR' then do
        parse var rest pnrRaw
        pnrNorm = strip(translate(pnrRaw))
        if left(pnrNorm,3) \= 'PNR' then do
            padded = substr('000'||pnrNorm,length('000'||pnrNorm)-2,3)
            pnr = 'PNR'||padded
        end
        else pnr = pnrNorm

        found = 0
        do j = 1 to PNRdata.0
            parse var PNRdata.j pnrEntry pname seat flightRec
            if pnrEntry = pnr then do
                say 'PNR '||pnrEntry||' => '||pname||' SEAT '||seat||' '||flightRec
                /* Display passenger type if exists */
                if paxType.j \= '' then
                    say '   Type: '||paxTypes.paxType.j
                /* Display FF info if exists */
                if ff.pnr \= '' then
                    say '   FF: '||ff.pnr||' ('||ff.pnr.airline||')'
                found = 1
                leave
            end
        end
        if found = 0 then say '*** PNR '||pnr||' not found.'
        iterate
    end

    /* --- Equipment Query Commands --- */
    if left(cmd,4) = 'W/EQ' then do
        if length(cmd) > 5 & substr(cmd,5,1) = '*' then
            eqQuery = substr(cmd,6)
        else do
            /* Show all equipment types */
            say 'Available Equipment Types:'
            say '------------------------'
            do i = 1 to words(eqDesc)
                eq = word(eqDesc, i)
                say left(eq,5) '-' left(eqDesc.eq,20) right(eqSeats.eq,4) 'seats'
            end
            iterate
        end
        
        /* Handle specific equipment query */
        eqQuery = strip(translate(eqQuery))
        eqName = eqDesc.eqQuery
        if eqName = '' then eqName = 'Unknown'
        say 'Flights with equipment '||eqQuery||' - '||eqName
        say ' #   ARLN    FLTN      DEPC/ARVC     DEPT      ARRT      AVST     EQTYP'
        say '-----------------------------------------------------------------------'
        idx = 0
        do i = 1 to matchList.0
            parse var matchList.i airline flight depc depd dept arrt arrc avst eqmt
            if eqmt \= eqQuery then iterate
            idx = idx + 1
            out = right(idx,2)
            out = out||'   '||left(airline,5)
            out = out||'   '||left(flight,7)
            out = out||'   '||left(depc||'/'||arrc,11)
            out = out||'   '||left(dept,7)
            out = out||'   '||left(arrt,7)
            out = out||'   '||left(avst,6)
            out = out||'   '||eqmt
            say out
        end
        if idx = 0 then say '*** NO FLIGHTS WITH THAT EQUIPMENT.'
        iterate
    end

    /* --- Flight Query --- */
    parse var cmd date from to time

    /* Basic input validation */
    if date = '' | from = '' | to = '' then do
        say '*** Invalid query format. Use: DATE FROM TO [TIME]'
        say '*** Example: 18OCT JFK ZRH 9A'
        iterate
    end

    /* Validate airport codes */
    if \isValidAirport(from) then do
        say '*** Invalid departure airport code: '||from
        say '*** Please use a valid 3-letter airport code (e.g. JFK, LHR, SIN)'
        iterate
    end

    if \isValidAirport(to) then do
        say '*** Invalid arrival airport code: '||to
        say '*** Please use a valid 3-letter airport code (e.g. JFK, LHR, SIN)'
        iterate
    end

    if from = to then do
        say '*** Departure and arrival airports cannot be the same'
        iterate
    end

    /* Show route information */
    say 'Route: '||from||' ('||getAirportName(from)||') to '||to||' ('||getAirportName(to)||')'

    /* Generate flights for this query */
    call generateFlights date, from, to, time

    /* Clear previous flight tracking */
    drop flightSeats.
    flightSeats.0 = 0

    say
    say date||' '||from||'/'||to||' ----------------------'
    say ' #   ARLN    FLTN      DEPC/ARVC     DEPT      ARRT      AVST     EQTYP'
    say '-----------------------------------------------------------------------'

    matchList.0 = 0
    if flights.0 = 0 then do
        say "NO FLIGHTS AVAILABLE FOR THAT QUERY"
        iterate
    end

    do i = 1 to flights.0
        parse var flights.i airline flight depc depd dept arrt arrc avst eqmt
        
        /* time-of-day filter if specified */
        match = 1
        if time \= '' then do
            parse var dept hr 3 mn 5 ap 6
            if \datatype(hr,'W') | \datatype(mn,'W') then iterate
            mins = (hr * 60) + mn
            if ap = 'P' & hr \= 12 then mins = mins + (12 * 60)
            if ap = 'A' & hr = 12 then mins = 0
            
            timeHr = substr(time,1,length(time)-1)
            timeAP = right(time,1)
            if \datatype(timeHr,'W') then iterate
            targetMins = (timeHr * 60)
            if timeAP = 'P' & timeHr \= 12 then targetMins = targetMins + (12 * 60)
            if timeAP = 'A' & timeHr = 12 then targetMins = 0
            
            /* Allow 2 hour window around requested time */
            if abs(mins - targetMins) > 120 then match = 0
        end

        if match then do
            idx = matchList.0 + 1
            matchList.0 = idx
            matchList.idx = airline flight depc depd dept arrt arrc avst eqmt

            /* Initialize seat tracking with numeric value */
            flightKey = airline||flight||depc||arrc||depd||dept
            numeric_avst = avst + 0  /* Force numeric conversion */
            flightSeats.flightKey = numeric_avst

            out = right(idx,2)
            out = out||'   '||left(airline,5)
            out = out||'   '||left(flight,7)
            out = out||'   '||left(depc||'/'||arrc,11)
            out = out||'   '||left(dept,7)
            out = out||'   '||left(arrt,7)
            out = out||'   '||left(avst,6)
            out = out||'   '||eqmt
            say out
        end
    end

    if matchList.0 = 0 then
        say "NO FLIGHTS AVAILABLE FOR THAT QUERY"
end

/* Function definitions */
genRandomNum: 
    parse arg min, max
    /* Use combination of time and random for better distribution */
    seed = (time('S') * 100 + right(time('M'), 2)) // 1000
    random = (seed * 17 + 23) // (max - min + 1)
    return min + random

checkTimeFormat:
    parse arg timeStr
    if timeStr = '' then return 1
    if length(timeStr) < 2 then return 0
    numPart = substr(timeStr,1,length(timeStr)-1)
    ampm = right(timeStr,1)
    if \datatype(numPart,'W') then return 0
    if \verify(ampm,'AP') = 0 then return 0
    if numPart < 1 | numPart > 12 then return 0
    return 1

getFlightTime:
    parse arg dep, arr
    /* Simple mock flight times - could be enhanced with real distances */
    if length(dep) = 3 & length(arr) = 3 then
        return 60 + genRandomNum(60, 720)  /* 1-12 hours */
    return 120  /* default 2 hours */

formatTime:
    parse arg mins
    if \datatype(mins,'W') then return '0000A'  /* Error case */
    hours = mins % 60
    minutes = mins // 60
    ampm = 'A'
    if hours >= 12 then do
        ampm = 'P'
        if hours > 12 then hours = hours - 12
    end
    if hours = 0 then hours = 12
    return right(hours,2,'0')||right(minutes,2,'0')||ampm

/* Function to determine appropriate equipment for route */
getRouteEquipment:
    parse arg dep, arr
    equipList = ''
    
    /* Get rough distance based on regions */
    depRegion = getAirportRegion(dep)
    arrRegion = getAirportRegion(arr)
    
    if depRegion = arrRegion then do
        /* Short-haul domestic/regional */
        if depRegion = 'NA' then
            return 'A320 A320 B738 B738 B738'  /* Favor narrowbodies for domestic */
        else
            return 'A320 B738 A320 B738 A320'  /* Regional mix */
    end
    
    /* Check if transatlantic */
    if (depRegion = 'NA' & arrRegion = 'EU') | (depRegion = 'EU' & arrRegion = 'NA') then
        return 'B789 A350 B77W B789 A350'  /* Widebodies for transatlantic */
    
    /* Check if transpacific */
    if (depRegion = 'NA' & arrRegion = 'AP') | (depRegion = 'AP' & arrRegion = 'NA') then
        return 'B789 B77W B77W B789 B77W'  /* Favor larger widebodies */
        
    /* Default long-haul mix */
    return 'B789 A350 B77W A350 B789'

/* Function to determine airport region */
getAirportRegion:
    parse arg code
    select
        when wordpos(code, 'JFK LAX ORD DFW DEN SFO LAS SEA MCO EWR MIA PHX IAH BOS MSP DTW FLL CLT LGA BWI SLC YYZ YVR YUL') > 0 then return 'NA'
        when wordpos(code, 'LHR CDG AMS FRA IST MAD BCN LGW MUC FCO SVO DME DUB ZRH CPH OSL ARN VIE BRU MXP') > 0 then return 'EU'
        when wordpos(code, 'PEK HND HKG ICN BKK SIN CGK KUL DEL BOM SYD MEL AKL KIX TPE MNL CAN PVG NRT') > 0 then return 'AP'
        when wordpos(code, 'DXB DOH AUH CAI JNB CPT TLV BAH RUH JED MCT') > 0 then return 'ME'
        when wordpos(code, 'GRU MEX BOG LIM SCL GIG EZE PTY CUN UIO') > 0 then return 'LA'
        otherwise return 'OT'
    end

/* Function to get a truly random number */
getRandomNum:
    parse arg min, max
    /* Use combination of time values and counter for randomization */
    numeric digits 20
    counter = random(1, 10000)  /* Get a random counter value */
    t = time('L')  /* Get time in long format */
    seed = (right(t, 8) * counter) // 1000000
    return trunc(min + (seed // (max - min + 1)))

/* Function to get next unique flight number */
getUniqueFlightNum:
    parse arg airline, minFlt, maxFlt, usedNums
    do forever
        /* Add some CPU work to change time value */
        do i = 1 to 100; nop; end
        flightNum = getRandomNum(minFlt, maxFlt)
        flightStr = right(flightNum, 4, '0')
        if wordpos(airline||flightStr, usedNums) = 0 then
            return flightStr
    end

generateFlights:
    parse arg date, depApt, arrApt, timeFilter

    /* Initialize result stem */
    flights.0 = 0
    usedNums = ''
    
    /* Get appropriate equipment list for this route */
    routeEquipment = getRouteEquipment(depApt, arrApt)
    numEquipment = words(routeEquipment)

    /* Generate 2-5 flights per airline */
    do a = 1 to airlines.0
        airline = airlines.a
        parse var flightNumRange.airline minFlt maxFlt
        
        /* For each airline, generate a different number of flights */
        numFlights = getRandomNum(2, 5)
        
        do f = 1 to numFlights
            /* Get unique flight number */
            flightNumStr = getUniqueFlightNum(airline, minFlt, maxFlt, usedNums)
            usedNums = usedNums airline||flightNumStr
            
            /* Generate departure time - spread throughout the day */
            do i = 1 to 100; nop; end  /* Force time change */
            depTime = getRandomNum(360, 1380)
            depTime = (depTime % 5) * 5  /* Round to nearest 5 minutes */
            
            /* Select equipment for this flight */
            do i = 1 to 100; nop; end  /* Force time change */
            eqIdx = getRandomNum(1, numEquipment)
            eqType = word(routeEquipment, eqIdx)
            
            /* Calculate arrival time based on route */
            flightDur = getFlightTime(depApt, arrApt)
            arrTime = depTime + flightDur
            
            /* Format times */
            depTimeStr = formatTime(depTime)
            arrTimeStr = formatTime(arrTime)
            
            /* Get seats for this equipment type */
            maxSeats = eqSeats.eqType
            if \datatype(maxSeats,'W') then maxSeats = 150  /* Default if not found */
            
            /* Generate random available seats */
            do i = 1 to 100; nop; end  /* Force time change */
            minSeats = trunc(maxSeats * 0.2)  /* At least 20% seats available */
            maxAvail = trunc(maxSeats * 0.95)  /* Up to 95% seats available */
            availSeats = getRandomNum(minSeats, maxAvail)
            
            /* Add to flights stem */
            idx = flights.0 + 1
            flights.0 = idx
            flights.idx = airline flightNumStr depApt date depTimeStr arrTimeStr arrApt availSeats eqType
        end
    end

    /* Sort flights by departure time */
    if flights.0 > 1 then do
        do i = 1 to flights.0 - 1
            do j = i + 1 to flights.0
                parse var flights.i ai fi di dti dpi ari avi ei
                parse var flights.j aj fj dj dtj dpj arj avj ej
                if dpi > dpj then do
                    temp = flights.i
                    flights.i = flights.j
                    flights.j = temp
                end
            end
        end
    end
    return

/* Function to validate airport code */
isValidAirport:
    parse arg code
    if code = '' then return 0
    if length(code) \= 3 then return 0
    if symbol('airports.'||code) = 'LIT' then return 0
    return 1

/* Function to get airport name */
getAirportName:
    parse arg code
    if symbol('airports.'||code) = 'LIT' then return ''
    return value('airports.'||code)

/* Function to safely convert to number */
toNumber:
    parse arg val
    val = strip(val)  /* Remove whitespace */
    if datatype(val, 'W') then
        return val + 0
    return -1

/* Display flight list with proper formatting */
displayFlights:
    parse arg date, from, to
    say
    say date||' '||from||'/'||to||' ----------------------'
    say ' #   ARLN    FLTN      DEPC/ARVC     DEPT      ARRT      AVST     EQTYP'
    say '-----------------------------------------------------------------------'
    
    do i = 1 to flights.0
        parse var flights.i airline flight depc depd dept arrt arrc avst eqmt
        
        /* time-of-day filter if specified */
        match = 1
        if time \= '' then do
            parse var dept hr 3 mn 5 ap 6
            if \datatype(hr,'W') | \datatype(mn,'W') then iterate
            mins = (hr * 60) + mn
            if ap = 'P' & hr \= 12 then mins = mins + (12 * 60)
            if ap = 'A' & hr = 12 then mins = 0
            
            timeHr = substr(time,1,length(time)-1)
            timeAP = right(time,1)
            if \datatype(timeHr,'W') then iterate
            targetMins = (timeHr * 60)
            if timeAP = 'P' & timeHr \= 12 then targetMins = targetMins + (12 * 60)
            if timeAP = 'A' & timeHr = 12 then targetMins = 0
            
            /* Allow 2 hour window around requested time */
            if abs(mins - targetMins) > 120 then match = 0
        end

        if match then do
            idx = matchList.0 + 1
            matchList.0 = idx
            /* Store numeric seat count in a separate field */
            matchList.idx = airline flight depc depd dept arrt arrc avst eqmt
            matchList.idx.seats = avst  /* Store seats as a separate numeric field */

            out = right(idx,2)
            out = out||'   '||left(airline,5)
            out = out||'   '||left(flight,7)
            out = out||'   '||left(depc||'/'||arrc,11)
            out = out||'   '||left(dept,7)
            out = out||'   '||left(arrt,7)
            out = out||'   '||left(avst,6)
            out = out||'   '||eqmt
            say out
        end
    end

    if matchList.0 = 0 then
        say "NO FLIGHTS AVAILABLE FOR THAT QUERY"
    return
