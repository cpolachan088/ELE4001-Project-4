#include <Arduino.h>
#include <SensirionI2cScd30.h>  
#include <DFRobot_SGP40.h>     
#include <sps30.h>             
#include <Wire.h>               
#include <WiFi.h>               
#include <HTTPClient.h>        
#include <esp_wpa2.h>           
#include "FS.h"                
#include "SD.h"                 
#include "SPI.h"               
#include "time.h"               

// Pin definitions for SD card connection
#define SD_MOSI     23
#define SD_MISO     19
#define SD_SCLK     18
#define SD_CS       5

// Setting test name for data logging
String testName = "demo";  

// Initialize sensor objects
SensirionI2cScd30 scd30;
DFRobot_SGP40 mySgp40;

// I2C addresses for sensors
#define SCD30_ADDRESS 0x61
#define SPS30_ADDRESS 0x69

// NTP server configuration for time synchronization
const char* ntpServer = "pool.ntp.org";
const long  gmtOffset_sec = 0;      // GMT offset (seconds)
const int   daylightOffset_sec = 3600;  // Daylight saving time offset (seconds)

// Flag to track SD card initialization status
bool sdCardInitialized = false; 

// Structure to store WiFi network configuration including enterprise authentication
struct WifiConfig {
   const char* ssid;
   const char* password; 
   const char* username;  // For WPA2 Enterprise
   bool isEnterprise;     // Flag to indicate if network uses WPA2 Enterprise
};

// Array of available WiFi networks to try connecting to
WifiConfig networks[] = {
    {"SSID1", "USERNAME", "PASSWORD1", true},
    {"SSID3", "PASSWORD2", nullptr, false},
    {"SSID3", "PASSWORD3", nullptr, false}
};

const int NETWORK_COUNT = sizeof(networks) / sizeof(networks[0]);
// Google Sheets web app URL for data logging
const char* serverURL = "https://script.google.com/macros/s/AKfycbz64eq7PgxM52bCXllspzZSBROSUcZCpQkDksHhB7PpBK8NFS3zla0DC7F_8QEr9b2A/exec";

// Function prototypes
void initSDCard();
void writeToSD(float pm25, float pm10, float co2, float voc, float temp, float humidity, bool isWarmUp);
String getTimeString();
String getDateString();

/**
 * Attempts to connect to a specific WiFi network
 * 
 * @param network WiFi configuration to connect to
 * @return true if connection successful, false otherwise
 */

bool connectToNetwork(WifiConfig network) {
   Serial.printf("\nAttempting to connect to %s...\n", network.ssid);
   WiFi.disconnect(true);
   delay(100);
   WiFi.mode(WIFI_STA);

   // Handle WPA2 Enterprise networks (like university networks)
   if (network.isEnterprise) {
       esp_wifi_sta_wpa2_ent_set_identity((uint8_t*)network.username, strlen(network.username));
       esp_wifi_sta_wpa2_ent_set_username((uint8_t*)network.username, strlen(network.username));
       esp_wifi_sta_wpa2_ent_set_password((uint8_t*)network.password, strlen(network.password));
       esp_wifi_sta_wpa2_ent_enable();
       WiFi.begin(network.ssid);
   } else {
       WiFi.begin(network.ssid, network.password);
   }

   int retry_count = 0;
   const int MAX_RETRIES = 30;

   // Wait for connection with timeout
   while (WiFi.status() != WL_CONNECTED && retry_count < MAX_RETRIES) {
       delay(500);
       Serial.print(".");
       retry_count++;
   }

   if (WiFi.status() == WL_CONNECTED) {
       Serial.printf("\nConnected to %s!\n", network.ssid);
       Serial.printf("IP Address: %s\n", WiFi.localIP().toString().c_str());
       return true;
   }

   Serial.printf("\nFailed to connect to %s\n", network.ssid);
   return false;
}

/**
 * Tries to connect to any of the configured WiFi networks
 * 
 * @return true if successfully connected to any network, false otherwise
 */

bool connectToAnyNetwork() {
   for (int i = 0; i < NETWORK_COUNT; i++) {
       if (connectToNetwork(networks[i])) {
           // Configure time using NTP
           configTime(gmtOffset_sec, daylightOffset_sec, ntpServer);
           return true;
       }
   }
   Serial.println("Failed to connect to any network");
   return false;
}

/**
 * Validates sensor readings to ensure they are within reasonable ranges
 * 
 * @param value The sensor reading to validate
 * @param min Minimum acceptable value
 * @param max Maximum acceptable value
 * @return true if reading is valid, false otherwise
 */

bool validateSensorData(float value, float min, float max) {
   return !isnan(value) && value >= min && value <= max;
}

/**
 * Sends sensor data to Google Sheets via web app
 * 
 * @param pm25 PM2.5 reading in μg/m³
 * @param pm10 PM10 reading in μg/m³
 * @param co2 CO2 reading in ppm
 * @param voc VOC index
 * @param temp Temperature in °C
 * @param humidity Relative humidity in %
 * @param isWarmUp Flag indicating if sensor is in warm-up phase
 */

void sendDataToGoogleSheets(float pm25, float pm10, float co2, float voc, float temp, float humidity, bool isWarmUp) {
   if (WiFi.status() == WL_CONNECTED) {
       HTTPClient http;
       http.begin(serverURL);
       http.addHeader("Content-Type", "application/json");

       // Mark data as warm-up if sensors are still warming up
       String currentTestName = isWarmUp ? "WARMUP_" + testName : testName;

       // Construct JSON payload with all sensor readings
       String jsonPayload = "{";
       jsonPayload += "\"Test\":\"" + currentTestName + "\",";
       jsonPayload += "\"PM2.5\":" + String(pm25) + ",";
       jsonPayload += "\"PM10\":" + String(pm10) + ",";
       jsonPayload += "\"CO2\":" + String(co2) + ",";
       jsonPayload += "\"VOC\":" + String(voc) + ",";
       jsonPayload += "\"Temperature\":" + String(temp) + ",";
       jsonPayload += "\"Humidity\":" + String(humidity);
       jsonPayload += "}";

       int httpResponseCode = http.POST(jsonPayload);

       if (httpResponseCode > 0) {
           Serial.println("Data sent successfully!");
           Serial.println("HTTP Response code: " + String(httpResponseCode));
       } else {
           Serial.println("Error sending data");
           Serial.println("HTTP Response code: " + String(httpResponseCode));
       }

       http.end();
   } else {
       Serial.println("WiFi not connected. Data not sent.");
       connectToAnyNetwork();  // Try to reconnect
   }
}

// Flags and timers for sensor warm-up period
bool isWarmingUp = true;
unsigned long warmupStartTime;
const unsigned long WARMUP_DURATION = 120000;  // 2-minute warm-up period in milliseconds

/**
 * Initializes the SD card for data logging
 */

void initSDCard() {
   Serial.println("Initializing SD card...");
   SPI.begin(SD_SCLK, SD_MISO, SD_MOSI, SD_CS);

   if (!SD.begin(SD_CS)) {
       Serial.println("SD Card initialization failed!");
       sdCardInitialized = false;
       return;
   }

   sdCardInitialized = true;
   Serial.println("SD Card initialized successfully");

   // Display SD card size
   uint64_t cardSize = SD.cardSize() / (1024 * 1024);
   Serial.print("SD Card Size: ");
   Serial.print((uint32_t)cardSize);
   Serial.println(" MB");

   // Create data directory if it doesn't exist
   if (!SD.exists("/data")) {
       if (SD.mkdir("/data")) {
           Serial.println("Data directory created");
       } else {
           Serial.println("Failed to create data directory");
       }
   }
}

/**
 * Writes sensor data to SD card in CSV format
 * 
 * @param pm25 PM2.5 reading in μg/m³
 * @param pm10 PM10 reading in μg/m³
 * @param co2 CO2 reading in ppm
 * @param voc VOC index
 * @param temp Temperature in °C
 * @param humidity Relative humidity in %
 * @param isWarmUp Flag indicating if sensor is in warm-up phase
 */

void writeToSD(float pm25, float pm10, float co2, float voc, float temp, float humidity, bool isWarmUp) {
   if (!sdCardInitialized) {
       Serial.println("SD card not initialized, skipping data logging");
       return;
   }

   String currentTestName = isWarmUp ? "WARMUP_" + testName : testName;

   // Create filename based on current date (YYYY-MM-DD.csv)
   String dateStr = getDateString();
   String filename = "/data/" + dateStr + ".csv";

   bool fileExists = SD.exists(filename);

   File dataFile = SD.open(filename, FILE_APPEND);

   if (dataFile) {
       // Write header if file is new
       if (!fileExists) {
           dataFile.println("Timestamp,Test,PM2.5,PM10,CO2,VOC,Temperature,Humidity");
       }

       String timeStr = getTimeString();

       // Format data as CSV row
       String dataString = timeStr + "," + 
                         currentTestName + "," +
                         String(pm25) + "," + 
                         String(pm10) + "," + 
                         String(co2) + "," + 
                         String(voc) + "," + 
                         String(temp) + "," + 
                         String(humidity);

       dataFile.println(dataString);
       dataFile.close();

       Serial.println("Data saved to SD: " + filename);
   } else {
       Serial.println("Error opening file on SD card: " + filename);
   }
}

/**
 * Gets formatted time string (HH:MM:SS) from NTP-synchronized clock
 * 
 * @return String containing formatted time
 */

String getTimeString() {
   struct tm timeinfo;
   if(!getLocalTime(&timeinfo)) {
       Serial.println("Failed to obtain time");
       return "00:00:00";
   }

   char timeStringBuff[9]; 
   strftime(timeStringBuff, sizeof(timeStringBuff), "%H:%M:%S", &timeinfo);
   return String(timeStringBuff);
}

/**
 * Gets formatted date string (YYYY-MM-DD) from NTP-synchronized clock
 * 
 * @return String containing formatted date
 */

String getDateString() {
   struct tm timeinfo;
   if(!getLocalTime(&timeinfo)) {
       Serial.println("Failed to obtain time");
       return "0000-00-00";
   }

   char dateStringBuff[11]; 
   strftime(dateStringBuff, sizeof(dateStringBuff), "%Y-%m-%d", &timeinfo);
   return String(dateStringBuff);
}

/**
 * Setup function: runs once at startup
 * Initializes hardware, sensors, connections and starts measurements
 */

void setup() {
   Serial.begin(115200);  // Initialize serial communication
   Wire.begin();          // Initialize I2C communication

   initSDCard();  // Initialize SD card

   // Initialize SCD30 CO2 sensor
   scd30.begin(Wire, 0x61);
   uint16_t error = scd30.startPeriodicMeasurement(0);

   // Initialize SGP40 VOC sensor
   if (mySgp40.begin(10000) != true) {
       Serial.println("Failed to initialize SGP40 sensor");
       while(1);
   }

   // Initialize SPS30 particulate matter sensor
   sensirion_i2c_init();
   if (sps30_probe() != 0) {
       Serial.println("Failed to initialize SPS30 sensor");
   }
   if (sps30_start_measurement() != 0) {
       Serial.println("Failed to start SPS30 measurement");
   }

   // Connect to WiFi
   if (!connectToAnyNetwork()) {
       Serial.println("Initial WiFi connection failed.");
   }

   // Start sensor warm-up period
   warmupStartTime = millis();
   Serial.println("Starting 2-minute warm-up period for all sensors...");
}

/**
 * Loop function: runs repeatedly after setup
 * Reads sensor data and logs it at regular intervals
 */

void loop() {
   static unsigned long lastReadingTime = 0;
   const unsigned long readingInterval = 60000;  // Take readings every minute (60000ms)

   // Check if warm-up period has ended
   if (isWarmingUp && millis() - warmupStartTime >= WARMUP_DURATION) {
       isWarmingUp = false;
       Serial.println("Warm-up complete!");
   }

   // Check if it's time to take a new reading
   if (millis() - lastReadingTime >= readingInterval) {
       // Read CO2, temperature and humidity from SCD30 sensor
       float co2 = 0, temp = 0, humidity = 0;
       uint16_t error = scd30.blockingReadMeasurementData(co2, temp, humidity);

       // Apply calibration offsets to temperature and humidity
       temp += -1;
       humidity += -6;

       // Update temperature and humidity for VOC sensor calculation
       if (validateSensorData(humidity, 0, 100) && validateSensorData(temp, -10, 50)) {
           mySgp40.setRhT(humidity, temp);
       }
       float voc = mySgp40.getVoclndex();

       // Read particulate matter data from SPS30 sensor
       float pm25 = 0, pm10 = 0;
       struct sps30_measurement m;
       if (sps30_read_measurement(&m) == 0) {
           pm25 = m.mc_2p5;
           pm10 = m.mc_10p0;
       }

       // Send data to Google Sheets
       sendDataToGoogleSheets(pm25, pm10, co2, voc, temp, humidity, isWarmingUp);

       // Log data to SD card
       writeToSD(pm25, pm10, co2, voc, temp, humidity, isWarmingUp);

       // Print current readings to serial monitor
       Serial.println("\nCurrent Readings " + String(isWarmingUp ? "(WARMUP)" : "") + ":");
       Serial.println("CO2: " + String(co2) + " ppm");
       Serial.println("Temperature: " + String(temp) + " °C");
       Serial.println("Humidity: " + String(humidity) + " %");
       Serial.println("VOC Index: " + String(voc));
       Serial.println("PM2.5: " + String(pm25) + " µg/m³");
       Serial.println("PM10: " + String(pm10) + " µg/m³");

       lastReadingTime = millis();  // Update timestamp for last reading
   }
}