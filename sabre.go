package main

/* Sabre Simulator in Go by moshix */
/* Original implementation by Ernietech in REXX for VM/370 CMS */
/* then expanded with dynamic data and more SABRE commands, first in REXX and
   now in Go */
/* June 2025 in Gaschurn, Austria */

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"os/exec"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"time"
)

// Data Structures
var airports = make(map[string]string)
var eqDesc = make(map[string]string)
var seatConfig = make(map[string]string)
var eqSeats = make(map[string]int)
var airlines = make(map[int]string)
var airlineCodes = make(map[string]string)
var flightNumRange = make(map[string]string)
var ffPartners = make(map[string]string)
var queueType = make(map[int]string)
var fareTypes = make(map[string]string)
var paxTypes = make(map[string]string)

var pnrData []PNR
var flightSeats = make(map[string]int)
var seatMap = make(map[string]map[string]string)
var seatMapInit = make(map[string]bool)
var ffData = make(map[string]FrequentFlyer)
var queues = make(map[int][]string)
var currentPNR = ""
var matchList []Flight

type PNR struct {
	PnrID     string
	Name      string
	Seat      string
	FlightNum int
	Cancelled bool
	PaxType   string
}

type Flight struct {
	Airline    string
	FlightNum  string
	DepCity    string
	Date       string
	DepTime    string
	ArrTime    string
	ArrCity    string
	AvailSeats int
	EquipType  string
	DepMinutes int // For sorting
}

type FrequentFlyer struct {
	Number  string
	Airline string
}

// Initialization
func initData() {
	// North America
	airports["ATL"] = "Atlanta Hartsfield-Jackson"
	airports["LAX"] = "Los Angeles International"
	airports["ORD"] = "Chicago O'Hare"
	airports["DFW"] = "Dallas/Fort Worth"
	airports["DEN"] = "Denver International"
	airports["JFK"] = "New York Kennedy"
	airports["SFO"] = "San Francisco International"
	airports["LAS"] = "Las Vegas McCarran"
	airports["SEA"] = "Seattle-Tacoma"
	airports["MCO"] = "Orlando International"
	airports["EWR"] = "Newark Liberty"
	airports["MIA"] = "Miami International"
	airports["PHX"] = "Phoenix Sky Harbor"
	airports["IAH"] = "Houston Bush"
	airports["BOS"] = "Boston Logan"
	airports["MSP"] = "Minneapolis-St Paul"
	airports["DTW"] = "Detroit Metro"
	airports["FLL"] = "Fort Lauderdale"
	airports["CLT"] = "Charlotte Douglas"
	airports["LGA"] = "New York LaGuardia"
	airports["BWI"] = "Baltimore/Washington"
	airports["SLC"] = "Salt Lake City"
	airports["YYZ"] = "Toronto Pearson"
	airports["YVR"] = "Vancouver International"
	airports["YUL"] = "Montreal Trudeau"

	// Europe
	airports["LHR"] = "London Heathrow"
	airports["CDG"] = "Paris Charles de Gaulle"
	airports["AMS"] = "Amsterdam Schiphol"
	airports["FRA"] = "Frankfurt International"
	airports["IST"] = "Istanbul International"
	airports["MAD"] = "Madrid Barajas"
	airports["BCN"] = "Barcelona El Prat"
	airports["LGW"] = "London Gatwick"
	airports["MUC"] = "Munich International"
	airports["FCO"] = "Rome Fiumicino"
	airports["SVO"] = "Moscow Sheremetyevo"
	airports["DME"] = "Moscow Domodedovo"
	airports["DUB"] = "Dublin International"
	airports["ZRH"] = "Zurich International"
	airports["CPH"] = "Copenhagen Kastrup"
	airports["OSL"] = "Oslo Gardermoen"
	airports["ARN"] = "Stockholm Arlanda"
	airports["VIE"] = "Vienna International"
	airports["BRU"] = "Brussels International"
	airports["MXP"] = "Milan Malpensa"

	// Asia Pacific
	airports["PEK"] = "Beijing Capital"
	airports["HND"] = "Tokyo Haneda"
	airports["DXB"] = "Dubai International"
	airports["HKG"] = "Hong Kong International"
	airports["ICN"] = "Seoul Incheon"
	airports["BKK"] = "Bangkok Suvarnabhumi"
	airports["SIN"] = "Singapore Changi"
	airports["CGK"] = "Jakarta Soekarno-Hatta"
	airports["KUL"] = "Kuala Lumpur International"
	airports["DEL"] = "Delhi Indira Gandhi"
	airports["BOM"] = "Mumbai International"
	airports["SYD"] = "Sydney Kingsford Smith"
	airports["MEL"] = "Melbourne International"
	airports["AKL"] = "Auckland International"
	airports["KIX"] = "Osaka Kansai"
	airports["TPE"] = "Taipei Taoyuan"
	airports["MNL"] = "Manila Ninoy Aquino"
	airports["CAN"] = "Guangzhou Baiyun"
	airports["PVG"] = "Shanghai Pudong"
	airports["NRT"] = "Tokyo Narita"

	// Middle East & Africa
	airports["DOH"] = "Doha Hamad"
	airports["AUH"] = "Abu Dhabi International"
	airports["CAI"] = "Cairo International"
	airports["JNB"] = "Johannesburg O.R. Tambo"
	airports["CPT"] = "Cape Town International"
	airports["TLV"] = "Tel Aviv Ben Gurion"
	airports["BAH"] = "Bahrain International"
	airports["RUH"] = "Riyadh King Khalid"
	airports["JED"] = "Jeddah King Abdulaziz"
	airports["MCT"] = "Muscat International"

	// Latin America
	airports["GRU"] = "Sao Paulo Guarulhos"
	airports["MEX"] = "Mexico City International"
	airports["BOG"] = "Bogota El Dorado"
	airports["LIM"] = "Lima Jorge Chavez"
	airports["SCL"] = "Santiago International"
	airports["GIG"] = "Rio de Janeiro Galeao"
	airports["EZE"] = "Buenos Aires Ezeiza"
	airports["PTY"] = "Panama City Tocumen"
	airports["CUN"] = "Cancun International"
	airports["UIO"] = "Quito International"

	// Equipment
	eqDesc["A320"] = "Airbus A320-100"
	eqDesc["B738"] = "Boeing 737-800"
	eqDesc["B789"] = "Boeing 787-9"
	eqDesc["A350"] = "Airbus A350-100"
	eqDesc["B77W"] = "Boeing 777-300ER"
	eqDesc["B747"] = "Boeing 747-800"
	eqDesc["B767"] = "Boeing 767-300"
	eqDesc["B757"] = "Boeing 757-200"
	eqDesc["MD981"] = "MD-8-81"

	// Format: ROWS|LAYOUT|EXIT ROWS|FIRST CLASS ROWS|WHEELCHAIR ROWS|BASSINET ROWS|BLOCKED SEATS
	seatConfig["A320"] = "27|ABC DEF|11 12|1-4|1 27|12|14B 14E"
	seatConfig["B738"] = "28|ABC DEF|14 15|1-4|1 28|15|16B 16E"
	seatConfig["B789"] = "42|ABC DEF GHJ|24 25|1-5|1 42|25|26E 26F"
	seatConfig["A350"] = "44|ABC DEF GHJ|24 25|1-5|1 44|25|26E 26F"
	seatConfig["B77W"] = "51|ABC DEFG HJK|24 25|1-4|1 51|25|26E 26F"
	seatConfig["B747"] = "58|ABC DEFG HJK|24 25|1-4|1 58|25|26E 26F"
	seatConfig["B767"] = "34|ABC DEF GH|20 21|1-3|1 34|21|22D 22E"
	seatConfig["B757"] = "25|ABC DEF|14 15|1-3|1 25|15|16B 16E"
	seatConfig["MD981"] = "23|ABC DE|10 11|1-2|1 23|11|12B 12D"

	eqSeats["A320"] = 158
	eqSeats["B738"] = 162
	eqSeats["B789"] = 290
	eqSeats["A350"] = 308
	eqSeats["B77W"] = 357
	eqSeats["B747"] = 412
	eqSeats["B767"] = 203
	eqSeats["B757"] = 152
	eqSeats["MD981"] = 109

	// Airlines
	airlines[1] = "AAL"
	airlines[2] = "DAL"
	airlines[3] = "LH"
	airlineCodes["AAL"] = "American Airlines"
	airlineCodes["DAL"] = "Delta Airlines"
	airlineCodes["LH"] = "Lufthansa"

	flightNumRange["AAL"] = "100 999"
	flightNumRange["DAL"] = "1000 1999"
	flightNumRange["LH"] = "400 799"

	// Partners
	ffPartners["AAL"] = "BA QF CX JL"
	ffPartners["DAL"] = "AF KL VS KE"
	ffPartners["LH"] = "AC OS SN LX"

	// Queues
	queueType[1] = "GENERAL"
	queueType[2] = "TICKETING"
	queueType[3] = "SCHEDULE"
	queueType[4] = "WAITLIST"
	queueType[5] = "SPECIAL"

	// Fares
	fareTypes["Y"] = "FULL"
	fareTypes["B"] = "FLEX"
	fareTypes["M"] = "ECON"
	fareTypes["Q"] = "DISC"

	// Passenger Types
	paxTypes["ADT"] = "ADULT"
	paxTypes["CHD"] = "CHILD"
	paxTypes["INF"] = "INFANT"
	paxTypes["SNR"] = "SENIOR"
	paxTypes["STU"] = "STUDENT"
}

// Main Function
func main() {
	rand.Seed(time.Now().UnixNano())
	initData()
	clearScreen()

	fmt.Println(strings.Repeat("-", 14), "SABRE Computer Reservation System (CRS)", strings.Repeat("-", 15))
	today := time.Now().Format("02.01.2006")
	fmt.Println(today, "WAFL:CONE")

	scanner := bufio.NewScanner(os.Stdin)

	// Sign-in loop
	for {
		fmt.Println()
		fmt.Println("* Enter Agent Sign-in (e.g. SIA*01762):")
		if !scanner.Scan() {
			return
		}
		input := strings.ToUpper(strings.TrimSpace(scanner.Text()))

		if strings.HasPrefix(input, "SIA*") {
			break
		}
		fmt.Println("*** Sign-in must start with SIA*")
	}

	fmt.Println()
	fmt.Println("* SABRE DEMO: Type HELP for instructions")

	// Main command loop
	for {
		fmt.Println()
		if !scanner.Scan() {
			break
		}
		cmd := strings.ToUpper(strings.TrimSpace(scanner.Text()))
		if cmd == "" {
			continue
		}

		parts := strings.Fields(cmd)
		verb := parts[0]

		switch {
		case verb == "HELP":
			showHelp()
		case verb == "SO*":
			fmt.Println("Agent Sign Out complete")
			return
		case strings.HasPrefix(cmd, "4G"):
			handleSeatMap(cmd)
		case verb == "PNR":
			handlePnrLookup(parts)
		case verb == "BOOK":
			handleBook(parts, scanner)
		case strings.HasPrefix(verb, "FF"):
			handleFrequentFlyer(cmd)
		case strings.HasPrefix(verb, "Q/"):
			handleQueue(cmd)
		case strings.HasPrefix(verb, "WP/"):
			handlePricing(cmd)
		case strings.HasPrefix(verb, "N/"):
			handleNameChange(cmd)
		case verb == "CANCEL":
			handleCancel(parts)
		case strings.HasPrefix(verb, "W/EQ"):
			handleEquipQuery(cmd)
		default:
			handleFlightQuery(parts)
		}
	}
}

// Command Handlers
func showHelp() {
	fmt.Println("QUERY FORMAT: 18OCT JFK ZRH (optional: 9A)")
	fmt.Println("BOOK <n>      Book flight Index number <n> (e.g. BOOK 1 or BOOK 3)")
	fmt.Println("CANCEL <PNR>  Cancel booking (e.g. CANCEL 001 or CANCEL PNR001)")
	fmt.Println("PNR <loc>     Display booking details (e.g. PNR 001 or PNR PNR001)")
	fmt.Println("W/EQ*<code>   Filter flights by equipment code (e.g. A320 or B738)")
	fmt.Println("FF<pnr>       Display FF info for PNR (e.g. FF001 or FFPNR001)")
	fmt.Println("FF<pnr>/<num> Add FF number to PNR (e.g. FF001/AA123456)")
	fmt.Println("FF<pnr>/*     Delete FF number from PNR")
	fmt.Println("Q/C           Display queue counts")
	fmt.Println("Q/P/<n>/<pnr> Place PNR in queue n")
	fmt.Println("WP/NCB <n>    Price and book lowest available fare for segment n")
	fmt.Println("WP/NI         Display alternate fare options")
	fmt.Println("N/ADD-<n>     Add passenger name (N/ADD-1 DOE/JOHN ADT)")
	fmt.Println("N/CHG-<n>     Change passenger name (N/CHG-1 DOE/JANE)")
	fmt.Println("4G<n>*        Display seat map for segment n")
	fmt.Println("4GPNR<pnr>    Display seat map for PNR")
	fmt.Println("4G...S<seat>  Assign seat (e.g., 4G1S2A or 4GPNR001S2A)")
	fmt.Println("SO*           Sign Out all Work Areas")
}

func handleFlightQuery(parts []string) {
	if len(parts) < 3 {
		fmt.Println("*** Invalid query format. Use: DATE FROM TO [TIME]")
		fmt.Println("*** Example: 18OCT JFK ZRH 9A")
		return
	}

	date, from, to := parts[0], parts[1], parts[2]
	var timeFilter string
	if len(parts) > 3 {
		timeFilter = parts[3]
	}

	if !isValidAirport(from) {
		fmt.Println("*** Invalid departure airport code:", from)
		return
	}
	if !isValidAirport(to) {
		fmt.Println("*** Invalid arrival airport code:", to)
		return
	}
	if from == to {
		fmt.Println("*** Departure and arrival airports cannot be the same")
		return
	}

	fmt.Printf("Route: %s (%s) to %s (%s)\n", from, getAirportName(from), to, getAirportName(to))

	flights := generateFlights(date, from, to)
	displayFlights(date, from, to, timeFilter, flights)
}

func handleBook(parts []string, scanner *bufio.Scanner) {
	if len(parts) < 2 {
		fmt.Println("*** Invalid command")
		return
	}
	num, err := strconv.Atoi(parts[1])
	if err != nil || num < 1 || num > len(matchList) {
		fmt.Println("*** Invalid flight number")
		return
	}

	flight := matchList[num-1]
	flightKey := getFlightKey(flight)

	seats, ok := flightSeats[flightKey]
	if !ok {
		// If not in map, initialize from original flight data
		seats = flight.AvailSeats
		flightSeats[flightKey] = seats
	}

	if seats <= 0 {
		fmt.Println("*** Flight is full")
		return
	}

	fmt.Println("* Enter passenger name as <first initial><last name> (no spaces):")
	scanner.Scan()
	pname := strings.ToUpper(scanner.Text())

	seat := findNextAvailableSeat(flight)
	if seat == "" {
		fmt.Println("*** No seats available on this flight.")
		return
	}

	pnrID := fmt.Sprintf("PNR%03d", len(pnrData)+1)
	pnr := PNR{
		PnrID:     pnrID,
		Name:      pname,
		Seat:      seat,
		FlightNum: num,
		PaxType:   "ADT",
	}
	pnrData = append(pnrData, pnr)
	flightSeats[flightKey]--

	fmt.Printf("BOOKED %s PASSENGER %s SEAT %s\n", pnrID, pname, seat)
	currentPNR = pnrID
}

func handleCancel(parts []string) {
	if len(parts) < 2 {
		fmt.Println("*** PNR number required")
		return
	}
	pnrID := normalizePnr(parts[1])
	found := false
	for i := range pnrData {
		if pnrData[i].PnrID == pnrID {
			if pnrData[i].Cancelled {
				fmt.Println("*** PNR already cancelled")
				found = true
				break
			}
			pnrData[i].Cancelled = true
			flight := matchList[pnrData[i].FlightNum-1]
			flightKey := getFlightKey(flight)
			flightSeats[flightKey]++
			// Free up seat
			if _, ok := seatMap[flightKey]; ok {
				delete(seatMap[flightKey], pnrData[i].Seat)
			}
			fmt.Println("CANCELLED booking", pnrID)
			found = true
			break
		}
	}
	if !found {
		fmt.Println("*** PNR", pnrID, "not found.")
	}
}

func handlePnrLookup(parts []string) {
	if len(parts) < 2 {
		fmt.Println("*** PNR number required")
		return
	}
	pnrID := normalizePnr(parts[1])
	found := false
	for _, pnr := range pnrData {
		if pnr.PnrID == pnrID {
			if pnr.Cancelled {
				flight := matchList[pnr.FlightNum-1]
				flightStr := fmt.Sprintf("%s%s %s-%s", flight.Airline, flight.FlightNum, flight.DepCity, flight.ArrCity)
				fmt.Printf("PNR %s => %s %s CANCELLED\n", pnr.PnrID, pnr.Name, flightStr)
			} else {
				flight := matchList[pnr.FlightNum-1]
				flightStr := fmt.Sprintf("%s%s %s-%s", flight.Airline, flight.FlightNum, flight.DepCity, flight.ArrCity)
				fmt.Printf("PNR %s => %s SEAT %s %s\n", pnr.PnrID, pnr.Name, pnr.Seat, flightStr)
				fmt.Printf("   Type: %s\n", paxTypes[pnr.PaxType])
				if ff, ok := ffData[pnr.PnrID]; ok {
					fmt.Printf("   FF: %s (%s)\n", ff.Number, ff.Airline)
				}
				currentPNR = pnr.PnrID
			}
			found = true
			break
		}
	}
	if !found {
		fmt.Println("*** PNR", pnrID, "not found.")
	}
}

func handleEquipQuery(cmd string) {
	parts := strings.Split(cmd, "*")
	if len(parts) < 2 || parts[1] == "" {
		fmt.Println("Available Equipment Types:")
		fmt.Println("------------------------")
		// Sort codes for consistent output
		var codes []string
		for code := range eqDesc {
			codes = append(codes, code)
		}
		sort.Strings(codes)
		for _, code := range codes {
			fmt.Printf("%-5s - %-20s %4d seats\n", code, eqDesc[code], eqSeats[code])
		}
		return
	}
	eqQuery := parts[1]
	eqName, ok := eqDesc[eqQuery]
	if !ok {
		eqName = "Unknown"
	}
	fmt.Printf("Flights with equipment %s - %s\n", eqQuery, eqName)
	fmt.Println(" #   ARLN    FLTN      DEPC/ARVC     DEPT      ARRT      AVST     EQTYP")
	fmt.Println(strings.Repeat("-", 71))

	foundCount := 0
	for i, f := range matchList {
		if f.EquipType == eqQuery {
			fmt.Printf("%2d   %-5s   %-7s   %-11s   %-7s   %-7s   %-6d   %s\n",
				i+1, f.Airline, f.FlightNum, f.DepCity+"/"+f.ArrCity, f.DepTime, f.ArrTime, f.AvailSeats, f.EquipType)
			foundCount++
		}
	}
	if foundCount == 0 {
		fmt.Println("*** NO FLIGHTS WITH THAT EQUIPMENT.")
	}
}

func handleFrequentFlyer(cmd string) {
	cmd = strings.TrimPrefix(cmd, "FF")
	parts := strings.Split(cmd, "/")
	pnrRaw := parts[0]
	pnrID := normalizePnr(pnrRaw)

	pnr, pnrIdx := findPNR(pnrID)
	if pnrIdx == -1 {
		fmt.Println("*** PNR", pnrID, "not found")
		return
	}
	if pnr.Cancelled {
		fmt.Println("*** PNR", pnrID, "is cancelled")
		return
	}

	if len(parts) == 1 { // Display FF
		if ff, ok := ffData[pnrID]; ok {
			fmt.Printf("FF: %s (%s) for %s\n", ff.Number, ff.Airline, pnrID)
		} else {
			fmt.Println("No FF number stored for", pnrID)
		}
	} else if len(parts) > 1 {
		ffNum := parts[1]
		if ffNum == "*" { // Delete FF
			delete(ffData, pnrID)
			fmt.Println("Frequent flyer number deleted for", pnrID)
		} else { // Add/Update FF
			ff := FrequentFlyer{
				Number:  ffNum,
				Airline: ffNum[:2],
			}
			ffData[pnrID] = ff
			fmt.Printf("Frequent flyer number %s added for %s\n", ffNum, pnrID)
		}
	}
}

func handleQueue(cmd string) {
	parts := strings.Split(cmd, "/")
	if len(parts) < 2 {
		fmt.Println("*** Invalid queue command")
		return
	}
	qCmd := parts[1]

	switch qCmd {
	case "C":
		fmt.Println("Queue Counts:")
		fmt.Println("------------")
		for i := 1; i <= len(queueType); i++ {
			qList := queues[i]
			fmt.Printf("Queue %d (%s): %d PNRs\n", i, queueType[i], len(qList))
			if len(qList) > 0 {
				fmt.Println("   PNRs:")
				for _, pnrID := range qList {
					fmt.Println("   -", pnrID)
				}
			}
		}
	case "P":
		if len(parts) < 4 {
			fmt.Println("*** Invalid format. Use Q/P/<n>/<pnr>")
			return
		}
		qNum, err := strconv.Atoi(parts[2])
		if err != nil || qNum < 1 || qNum > len(queueType) {
			fmt.Println("*** Invalid queue number")
			return
		}
		pnrID := normalizePnr(parts[3])
		_, pnrIdx := findPNR(pnrID)
		if pnrIdx == -1 {
			fmt.Println("*** PNR", pnrID, "not found")
			return
		}
		// Add to queue
		queues[qNum] = append(queues[qNum], pnrID)
		fmt.Printf("PNR %s placed in queue %d (%s)\n", pnrID, qNum, queueType[qNum])
	default:
		fmt.Println("*** Invalid queue command")
	}
}

func handlePricing(cmd string) {
	parts := strings.Split(cmd, "/")
	if len(parts) < 2 {
		fmt.Println("*** Invalid pricing command")
		return
	}
	pCmd := parts[1]

	switch pCmd {
	case "NCB": // Price and book lowest
		if len(parts) < 3 {
			fmt.Println("*** Segment number required")
			return
		}
		fmt.Println("Searching for lowest available fare...")
		// Mock implementation
		for code, name := range fareTypes {
			price := 100 + rand.Intn(901)
			fmt.Printf("%s class (%s): $%d.00\n", code, name, price)
		}
	case "NI": // Display alternate fares
		fmt.Println("Searching for alternative fares...")
		// Mock implementation
		for code, name := range fareTypes {
			price := 100 + rand.Intn(901)
			fmt.Printf("%s class (%s): $%d.00\n", code, name, price)
		}
	default:
		fmt.Println("*** Invalid pricing command. Use WP/NCB <segment> or WP/NI")
	}
}

func handleNameChange(cmd string) {
	cmd = strings.TrimPrefix(cmd, "N/")
	parts := strings.Split(cmd, "-")
	if len(parts) < 2 {
		fmt.Println("*** Invalid name command.")
		return
	}
	cmdType := parts[0]
	rest := strings.Join(parts[1:], "-")

	paxData := strings.Fields(rest)
	paxNum, err := strconv.Atoi(paxData[0])
	if err != nil {
		fmt.Println("*** Invalid passenger number")
		return
	}

	if paxNum < 1 || paxNum > len(pnrData) {
		fmt.Println("*** Invalid PNR number")
		return
	}

	pnrIdx := paxNum - 1
	pnr := &pnrData[pnrIdx]

	switch cmdType {
	case "ADD", "CHG":
		nameParts := strings.Split(paxData[1], "/")
		if len(nameParts) < 2 {
			fmt.Println("*** Invalid name format. Use LAST/FIRST")
			return
		}
		newName := nameParts[0] + "/" + nameParts[1]
		pnr.Name = newName

		if cmdType == "ADD" {
			paxType := "ADT"
			if len(paxData) > 2 {
				if _, ok := paxTypes[paxData[2]]; ok {
					paxType = paxData[2]
				} else {
					fmt.Println("*** Invalid passenger type. Defaulting to ADT.")
				}
			}
			pnr.PaxType = paxType
			fmt.Printf("Added/Updated passenger: %s Type: %s to %s\n", pnr.Name, paxTypes[pnr.PaxType], pnr.PnrID)
		} else { // CHG
			fmt.Printf("Changed passenger name to: %s in %s\n", pnr.Name, pnr.PnrID)
		}
	default:
		fmt.Println("*** Invalid name command. Use N/ADD-<n> or N/CHG-<n>")
	}
}

func handleSeatMap(cmd string) {
	cmd = strings.TrimPrefix(cmd, "4G")

	var flight Flight
	var pnr *PNR

	// Seat assignment command
	if strings.Contains(cmd, "S") {
		parts := strings.Split(cmd, "S")
		if len(parts) < 2 {
			fmt.Println("*** Invalid seat assignment format.")
			return
		}

		target := parts[0]
		seatAssign := parts[1]

		var pnrID string
		if strings.HasPrefix(target, "PNR") {
			pnrID = normalizePnr(strings.TrimPrefix(target, "PNR"))
		} else { // 4G1S2A format, use current PNR
			pnrID = currentPNR
		}

		if pnrID == "" {
			fmt.Println("*** NO ACTIVE PNR - CANNOT ASSIGN SEAT")
			return
		}

		pnr, pnrIdx := findPNR(pnrID)
		if pnrIdx == -1 {
			fmt.Println("*** PNR", pnrID, "NOT FOUND")
			return
		}
		flight = matchList[pnr.FlightNum-1]

		flightKey := getFlightKey(flight)
		config := getSeatConfig(flight.EquipType)

		// Validate seat
		var seatRow int
		var seatLetter string
		for i, r := range seatAssign {
			if r < '0' || r > '9' {
				seatRow, _ = strconv.Atoi(seatAssign[:i])
				seatLetter = seatAssign[i:]
				break
			}
		}
		if seatRow == 0 || seatLetter == "" {
			fmt.Println("*** Invalid seat format - must be ROW+LETTER (e.g., 2A)")
			return
		}

		if seatRow < 1 || seatRow > config.rows {
			fmt.Printf("*** Invalid row number %d for %s\n", seatRow, eqDesc[flight.EquipType])
			return
		}
		if !strings.Contains(config.layout, seatLetter) {
			fmt.Printf("*** Invalid seat letter %s for %s\n", seatLetter, eqDesc[flight.EquipType])
			return
		}

		// Check availability
		newSeat := fmt.Sprintf("%d%s", seatRow, seatLetter)
		if status, ok := seatMap[flightKey][newSeat]; ok && status == "X" {
			fmt.Println("*** Seat", newSeat, "is not available")
			return
		}
		if strings.Contains(config.blockedSeats, newSeat) {
			fmt.Println("*** Seat", newSeat, "is blocked")
			return
		}

		// Release old seat
		if pnr.Seat != "" && seatMap[flightKey] != nil {
			delete(seatMap[flightKey], pnr.Seat)
		}

		// Assign new seat
		if seatMap[flightKey] == nil {
			seatMap[flightKey] = make(map[string]string)
		}
		seatMap[flightKey][newSeat] = "X"
		pnrData[pnrIdx].Seat = newSeat

		fmt.Printf("Seat changed to %s for %s\n", newSeat, pnrID)
		return
	}

	// Seat map display command
	if strings.HasPrefix(cmd, "PNR") {
		pnrID := normalizePnr(strings.TrimPrefix(cmd, "PNR"))
		var pnrIdx int
		pnr, pnrIdx = findPNR(pnrID)
		if pnrIdx == -1 {
			fmt.Println("*** PNR", pnrID, "NOT FOUND")
			return
		}
		currentPNR = pnrID
	}

	if currentPNR == "" {
		fmt.Println("*** NO ACTIVE PNR - DISPLAY PNR FIRST")
		return
	}

	pnr, pnrIdx := findPNR(currentPNR)
	if pnrIdx == -1 {
		fmt.Println("*** PNR", currentPNR, "NOT FOUND")
		return
	}
	flight = matchList[pnr.FlightNum-1]

	displaySeatMapForFlight(flight)
}

func displaySeatMapForFlight(f Flight) {
	flightKey := getFlightKey(f)
	config := getSeatConfig(f.EquipType)
	if config.rows == 0 {
		fmt.Println("*** Invalid equipment type:", f.EquipType)
		return
	}

	// Initialize seat map with random occupied seats if not already done
	if !seatMapInit[flightKey] {
		totalSeats := eqSeats[f.EquipType]
		availSeats := f.AvailSeats
		seatsToOccupy := totalSeats - availSeats

		if seatsToOccupy > 0 {
			allSeats := []string{}
			for r := 1; r <= config.rows; r++ {
				for _, c := range config.layout {
					if c == ' ' {
						continue
					}
					seatCode := fmt.Sprintf("%d%c", r, c)
					if strings.Contains(config.blockedSeats, seatCode) {
						continue
					}
					allSeats = append(allSeats, seatCode)
				}
			}
			rand.Shuffle(len(allSeats), func(i, j int) { allSeats[i], allSeats[j] = allSeats[j], allSeats[i] })

			if seatMap[flightKey] == nil {
				seatMap[flightKey] = make(map[string]string)
			}
			for i := 0; i < seatsToOccupy && i < len(allSeats); i++ {
				seatMap[flightKey][allSeats[i]] = "X"
			}
		}
		seatMapInit[flightKey] = true
	}

	fmt.Printf("SEAT MAP FOR %s%s %s\n", f.Airline, f.FlightNum, eqDesc[f.EquipType])
	fmt.Printf("DATE: %s SEGMENT: 1\n\n", f.Date)

	// Display header
	fmt.Printf("   %s\n", config.layout)

	// Display map
	for r := 1; r <= config.rows; r++ {
		fmt.Printf("%2d ", r)
		for _, c := range config.layout {
			if c == ' ' {
				fmt.Print(" ")
				continue
			}
			seatCode := fmt.Sprintf("%d%c", r, c)
			status := "A" // Available

			if s, ok := seatMap[flightKey][seatCode]; ok {
				status = s // Occupied
			} else if strings.Contains(config.blockedSeats, seatCode) {
				status = "M" // Blocked Middle
			} else if strings.Contains(config.firstRows, fmt.Sprintf(" %d ", r)) {
				status = "F" // First Class
			} else if strings.Contains(config.exitRows, fmt.Sprintf(" %d ", r)) {
				status = "E" // Exit
			} else if strings.Contains(config.wheelRows, fmt.Sprintf(" %d ", r)) {
				status = "W" // Wheelchair
			} else if strings.Contains(config.bassRows, fmt.Sprintf(" %d ", r)) {
				status = "C" // Bassinet
			}

			fmt.Print(status)
		}
		fmt.Println()
	}
	fmt.Println()
	fmt.Println("A=AVAILABLE  X=OCCUPIED  E=EXIT ROW  F=FIRST CLASS")
	fmt.Println("B=BLOCKED  W=WHEELCHAIR  C=BASSINET  M=BLOCKED MIDDLE")
}

// Flight Generation and Display
func generateFlights(date, depApt, arrApt string) []Flight {
	var flights []Flight
	usedNums := make(map[string]bool)
	routeEquipment := getRouteEquipment(depApt, arrApt)

	for i := 1; i <= len(airlines); i++ {
		airline := airlines[i]
		numFlights := rand.Intn(4) + 2 // 2-5 flights

		for f := 0; f < numFlights; f++ {
			flightNumStr := getUniqueFlightNum(airline, usedNums)
			usedNums[airline+flightNumStr] = true

			depTimeMins := rand.Intn(1380-360) + 360 // 6:00 to 23:00
			depTimeMins = (depTimeMins / 5) * 5      // Round to 5 mins

			eqType := routeEquipment[rand.Intn(len(routeEquipment))]
			flightDur := getFlightTime(depApt, arrApt)
			arrTimeMins := depTimeMins + flightDur

			maxSeats, _ := eqSeats[eqType]
			minSeats := int(float64(maxSeats) * 0.2)
			maxAvail := int(float64(maxSeats) * 0.95)
			availSeats := rand.Intn(maxAvail-minSeats+1) + minSeats

			flights = append(flights, Flight{
				Airline:    airline,
				FlightNum:  flightNumStr,
				DepCity:    depApt,
				Date:       date,
				DepTime:    formatTime(depTimeMins),
				ArrTime:    formatTime(arrTimeMins),
				ArrCity:    arrApt,
				AvailSeats: availSeats,
				EquipType:  eqType,
				DepMinutes: depTimeMins,
			})
		}
	}

	// Sort flights by departure time
	sort.Slice(flights, func(i, j int) bool {
		return flights[i].DepMinutes < flights[j].DepMinutes
	})
	return flights
}

func displayFlights(date, from, to, timeFilter string, flights []Flight) {
	fmt.Println()
	fmt.Printf("%s %s/%s ----------------------\n", date, from, to)
	fmt.Println(" #   ARLN    FLTN      DEPC/ARVC     DEPT      ARRT      AVST     EQTYP")
	fmt.Println(strings.Repeat("-", 71))

	matchList = nil // Clear previous list

	var targetMins = -1
	if timeFilter != "" {
		ampm := timeFilter[len(timeFilter)-1:]
		hrStr := timeFilter[:len(timeFilter)-1]
		hr, err := strconv.Atoi(hrStr)
		if err == nil {
			targetMins = hr * 60
			if ampm == "P" && hr != 12 {
				targetMins += 12 * 60
			}
			if ampm == "A" && hr == 12 {
				targetMins = 0 // Midnight
			}
		}
	}

	for _, f := range flights {
		if targetMins != -1 {
			diff := f.DepMinutes - targetMins
			if diff < 0 {
				diff = -diff
			}
			if diff > 120 { // 2 hour window
				continue
			}
		}

		matchList = append(matchList, f)
		idx := len(matchList)
		flightKey := getFlightKey(f)
		flightSeats[flightKey] = f.AvailSeats // Initialize seat count

		fmt.Printf("%2d   %-5s   %-7s   %-11s   %-7s   %-7s   %-6d   %s\n",
			idx, f.Airline, f.FlightNum, f.DepCity+"/"+f.ArrCity, f.DepTime, f.ArrTime, f.AvailSeats, f.EquipType)
	}

	if len(matchList) == 0 {
		fmt.Println("NO FLIGHTS AVAILABLE FOR THAT QUERY")
	}
}

// Utility Functions
func clearScreen() {
	if runtime.GOOS == "windows" {
		cmd := exec.Command("cmd", "/c", "cls")
		cmd.Stdout = os.Stdout
		cmd.Run()
	} else {
		cmd := exec.Command("clear")
		cmd.Stdout = os.Stdout
		cmd.Run()
	}
}

func isValidAirport(code string) bool {
	_, ok := airports[code]
	return ok && len(code) == 3
}

func getAirportName(code string) string {
	return airports[code]
}

func getRouteEquipment(dep, arr string) []string {
	depRegion := getAirportRegion(dep)
	arrRegion := getAirportRegion(arr)

	if depRegion == arrRegion {
		if depRegion == "NA" {
			return []string{"A320", "A320", "B738", "B738", "B738"}
		}
		return []string{"A320", "B738", "A320", "B738", "A320"}
	}
	if (depRegion == "NA" && arrRegion == "EU") || (depRegion == "EU" && arrRegion == "NA") {
		return []string{"B789", "A350", "B77W", "B789", "A350"}
	}
	if (depRegion == "NA" && arrRegion == "AP") || (depRegion == "AP" && arrRegion == "NA") {
		return []string{"B789", "B77W", "B77W", "B789", "B77W"}
	}
	return []string{"B789", "A350", "B77W", "A350", "B789"}
}

func getAirportRegion(code string) string {
	na := "JFK LAX ORD DFW DEN SFO LAS SEA MCO EWR MIA PHX IAH BOS MSP DTW FLL CLT LGA BWI SLC YYZ YVR YUL"
	eu := "LHR CDG AMS FRA IST MAD BCN LGW MUC FCO SVO DME DUB ZRH CPH OSL ARN VIE BRU MXP"
	ap := "PEK HND HKG ICN BKK SIN CGK KUL DEL BOM SYD MEL AKL KIX TPE MNL CAN PVG NRT"
	me := "DXB DOH AUH CAI JNB CPT TLV BAH RUH JED MCT"
	la := "GRU MEX BOG LIM SCL GIG EZE PTY CUN UIO"

	if strings.Contains(na, code) {
		return "NA"
	}
	if strings.Contains(eu, code) {
		return "EU"
	}
	if strings.Contains(ap, code) {
		return "AP"
	}
	if strings.Contains(me, code) {
		return "ME"
	}
	if strings.Contains(la, code) {
		return "LA"
	}
	return "OT"
}

func getUniqueFlightNum(airline string, used map[string]bool) string {
	ranges := strings.Fields(flightNumRange[airline])
	min, _ := strconv.Atoi(ranges[0])
	max, _ := strconv.Atoi(ranges[1])
	for {
		num := rand.Intn(max-min+1) + min
		numStr := fmt.Sprintf("%04d", num)
		if !used[airline+numStr] {
			return numStr
		}
	}
}

func getFlightTime(dep, arr string) int {
	return 60 + rand.Intn(661) // 1-12 hours
}

func formatTime(mins int) string {
	hours := mins / 60
	minutes := mins % 60
	ampm := "A"
	if hours >= 12 {
		ampm = "P"
		if hours > 12 {
			hours -= 12
		}
	}
	if hours == 0 {
		hours = 12
	}
	return fmt.Sprintf("%02d%02d%s", hours, minutes, ampm)
}

func getFlightKey(f Flight) string {
	return fmt.Sprintf("%s%s%s%s%s%s", f.Airline, f.FlightNum, f.DepCity, f.ArrCity, f.Date, f.DepTime)
}

func findNextAvailableSeat(f Flight) string {
	flightKey := getFlightKey(f)
	config := getSeatConfig(f.EquipType)

	if _, ok := seatMap[flightKey]; !ok {
		seatMap[flightKey] = make(map[string]string)
	}

	for r := 1; r <= config.rows; r++ {
		for _, c := range config.layout {
			if c == ' ' {
				continue
			}
			seatCode := fmt.Sprintf("%d%c", r, c)
			if strings.Contains(config.blockedSeats, seatCode) {
				continue
			}
			if _, occupied := seatMap[flightKey][seatCode]; !occupied {
				seatMap[flightKey][seatCode] = "X"
				return seatCode
			}
		}
	}
	return ""
}

func normalizePnr(pnrStr string) string {
	pnrStr = strings.ToUpper(pnrStr)
	if strings.HasPrefix(pnrStr, "PNR") {
		return pnrStr
	}
	num, err := strconv.Atoi(pnrStr)
	if err != nil {
		return "PNR" + pnrStr // Return as is if not numeric
	}
	return fmt.Sprintf("PNR%03d", num)
}

func findPNR(pnrID string) (*PNR, int) {
	for i := range pnrData {
		if pnrData[i].PnrID == pnrID {
			return &pnrData[i], i
		}
	}
	return nil, -1
}

type seatConfigParts struct {
	rows         int
	layout       string
	exitRows     string
	firstRows    string
	wheelRows    string
	bassRows     string
	blockedSeats string
}

func getSeatConfig(eqType string) seatConfigParts {
	configStr, ok := seatConfig[eqType]
	if !ok {
		return seatConfigParts{}
	}

	parts := strings.Split(configStr, "|")
	rows, _ := strconv.Atoi(parts[0])

	// Pad row numbers with spaces for accurate word-based searching
	padRows := func(s string) string {
		var padded []string
		for _, w := range strings.Fields(s) {
			padded = append(padded, " "+w+" ")
		}
		return strings.Join(padded, "")
	}

	return seatConfigParts{
		rows:         rows,
		layout:       parts[1],
		exitRows:     padRows(parts[2]),
		firstRows:    padRows(parts[3]),
		wheelRows:    padRows(parts[4]),
		bassRows:     padRows(parts[5]),
		blockedSeats: parts[6],
	}
}
