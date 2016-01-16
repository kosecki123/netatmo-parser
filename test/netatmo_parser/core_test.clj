(ns netatmo-parser.core-test
  (:require [clojure.data.json :as json]
            [clojure.test :refer :all]
            [netatmo-parser.core :refer :all]))

(deftest get-measurement-test
  (testing "Should return the temp"
    (let [ measure "[{
                    \"res\": {
                        \"1452254076\": [
                            -13.7,
                            75
                        ]
                    },
                    \"type\": [
                        \"temperature\",
                        \"humidity\"
                    ]
                },
                {
                    \"rain_60min\": 0,
                    \"rain_24h\": 0,
                    \"rain_live\": 0,
                    \"rain_timeutc\": 1452253764
                }
           ]"
           measure_deserialized (json/read-json measure)
           result (get-measure-results-by-type measure_deserialized)
           ]
    (println result)
    (is (= (:temperature result) -13.7))
    (is (= (:rain result) 0))
      )))

(deftest get-rain-data
  (testing "Should read the rain data from the json"
    (let [
           data "{
            \"_id\": \"70:ee:50:02:1e:48\",
            \"place\": {
                \"location\": [
                    7.33007,
                    51.34655
                ],
                \"altitude\": 236,
                \"timezone\": \"Europe/Berlin\"
            },
            \"mark\": 12,
            \"measures\": {
                \"02:00:00:02:22:58\": {
                    \"res\": {
                        \"1452514493\": [
                            6.7,
                            84
                        ]
                    },
                    \"type\": [
                        \"temperature\",
                        \"humidity\"
                    ]
                },
                \"05:00:00:00:c5:6a\": {
                    \"rain_60min\": 2.828,
                    \"rain_24h\": 4.141,
                    \"rain_live\": 0.202,
                    \"rain_timeutc\": 1452514519
                },
                \"70:ee:50:02:1e:48\": {
                    \"res\": {
                        \"1452514525\": [
                            990.6
                        ]
                    },
                    \"type\": [
                        \"pressure\"
                    ]
                }
            },
            \"modules\": [
                \"02:00:00:02:22:58\",
                \"05:00:00:00:c5:6a\"
            ]
        }"
           result-body (json/read-json data)
           m (get-measure-values result-body)
           result 0.202
           ]
      (println result-body)
      (println m)
      (is (= result 0.202)))))

(deftest get-lat-lon
  (testing "Should return coordinates from the json file"
    (let [ location "{
   \"results\" : [
      {
         \"address_components\" : [
            {
               \"long_name\" : \"Miami\",
               \"short_name\" : \"Miami\",
               \"types\" : [ \"locality\", \"political\" ]
            },
            {
               \"long_name\" : \"Hrabstwo Miami-Dade\",
               \"short_name\" : \"Hrabstwo Miami-Dade\",
               \"types\" : [ \"administrative_area_level_2\", \"political\" ]
            },
            {
               \"long_name\" : \"Floryda\",
               \"short_name\" : \"FL\",
               \"types\" : [ \"administrative_area_level_1\", \"political\" ]
            },
            {
               \"long_name\" : \"Stany Zjednoczone\",
               \"short_name\" : \"US\",
               \"types\" : [ \"country\", \"political\" ]
            }
         ],
         \"formatted_address\" : \"Miami, Floryda, Stany Zjednoczone\",
         \"geometry\" : {
            \"bounds\" : {
               \"northeast\" : {
                  \"lat\" : 25.8556059,
                  \"lng\" : -80.14240029999999
               },
               \"southwest\" : {
                  \"lat\" : 25.709042,
                  \"lng\" : -80.31975989999999
               }
            },
            \"location\" : {
               \"lat\" : 25.7616798,
               \"lng\" : -80.1917902
            },
            \"location_type\" : \"APPROXIMATE\",
            \"viewport\" : {
               \"northeast\" : {
                  \"lat\" : 25.8556059,
                  \"lng\" : -80.14240029999999
               },
               \"southwest\" : {
                  \"lat\" : 25.709042,
                  \"lng\" : -80.31975989999999
               }
            }
         },
         \"place_id\" : \"ChIJEcHIDqKw2YgRZU-t3XHylv8\",
         \"types\" : [ \"locality\", \"political\" ]
      }
   ],
   \"status\" : \"OK\"
}"
           result (get-location-from-body (deserialize-body {:body location}))
           ]

    (is (= result {:lat_ne 25.8556059, :lon_ne -80.14240029999999, :lon_sw -80.31975989999999, :lat_sw 25.709042})))))
