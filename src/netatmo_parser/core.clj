(ns netatmo-parser.core
  (:require [clojure.data.json :as json])
  (:require [clojure.java io])
  (:require [clj-http.client :as client])
  (:require [clojure.string :as string])
  (:gen-class))

(def maps-api-url "http://maps.googleapis.com/maps/api/geocode/json?address=")
(def netatmo-api-url "https://api.netatmo.net/api/getpublicdata")
(def netatmo-token-url "https://api.netatmo.net/oauth2/token")

(defn read-resource-string [path]
  (slurp (clojure.java.io/file path)))

(defn read-config []
  (read-string (read-resource-string "resources/config.edn")))

(defn deserialize-body [response]
  (json/read-json (:body response)))

(defn get-token []
  (let [query {:form-params (read-config) :insecure? true}
        response (client/post netatmo-token-url query)
        body (deserialize-body response)]
    (:access_token body)))

(defn get-netatmo-data [{ne_lat :lat_ne, ne_lon :lon_ne, sw_lon :lon_sw , sw_lat :lat_sw}]
  (let [token (get-token)
        query (str netatmo-api-url "?access_token=" token "&lat_ne=" ne_lat "&lon_ne=" ne_lon "&lat_sw=" sw_lat "&lon_sw=" sw_lon)
        response (client/get query {:insecure? true})]
    (deserialize-body response)))

(def first-vals (comp first vals))

(defn get-measure-values [{modules :modules, measures :measures}]
  (let [devices (map keyword modules)
        measure-values (select-keys measures devices)]
    (vals measure-values)))

(defn index-of [item coll]
  (count (take-while (partial not= item) coll)))

(defn get-temperature [{:keys [type res]}]
  (let [value-index (index-of "temperature" type)
        all-values (first-vals res)]
    (nth all-values value-index)))

(defn get-rain [res]
  (:rain_live (first res)))

(defn get-measure-results-by-type [all-measures]
  (let [measures-split-by-device ((juxt filter remove) #(contains? % :type) all-measures)
        measures-with-temp (ffirst measures-split-by-device)
        measures-with-rain (fnext measures-split-by-device)
        values-temperature (get-temperature measures-with-temp)
        values-rain (get-rain measures-with-rain)]
    {:temperature values-temperature, :rain values-rain}))

(defn get-location-from-body [{:keys [results]}]
  (let [area (get-in (first results) [:geometry :bounds])
        ne (:northeast area)
        ne_lat (get ne :lat)
        ne_lon (get ne :lng)
        sw (:southwest area)
        sw_lat (get sw :lat)
        sw_lon (get sw :lng)]
    {:lat_ne ne_lat, :lon_ne ne_lon, :lon_sw sw_lon, :lat_sw sw_lat}))

(defn get-location [city]
  (let [url (str maps-api-url city)
        response (client/get url)
        body (deserialize-body response)]
    (get-location-from-body body)))

(defn average [numbers]
  (when (seq numbers)
    (/ (apply + numbers) (count numbers))))

(defn -main
  "Find the city location and getting temp and rain data from Netatmo"
  [& args]
  (let [city (first args)
        city-location (get-location city)
        result-body (:body (get-netatmo-data city-location))
        extracted-measures (map #(get-measure-values %) result-body)
        measured-values (map get-measure-results-by-type extracted-measures)
        temparatures (filter some? (map #(:temperature %) measured-values))
        rains (filter some? (map #(:rain %) measured-values))]
    (println city "location" city-location)
    (println "Average temperature is" (average temparatures) "based on" (count temparatures) "results")
    (println "Current average rain drop is"  (average rains) "based on" (count rains) "results")))
