(ns aisong.note-sequences
  (:require [clojure.repl :refer :all]
            [leipzig.live :as live]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojure.walk :refer [keywordize-keys stringify-keys]]
            [leipzig.melody :refer :all]
            [overtone.music.pitch :as pitch]
            ))

(defn gcd [a b]
        (if (zero? b)
          a
          (recur b (mod a b))))


(defn lcm
      [a b]
  (/ (* a b) (gcd a b)))

(defn phrase2noteseq-note
  [note stepsPerQuarter]
  (let [quantizedStartStep (-> note :time
                               (* stepsPerQuarter))
        quantizedEndStep   (-> note
                               :duration
                               (* stepsPerQuarter)
                               (+  quantizedStartStep))]
    {:pitch (:pitch note)
     :quantizedStartStep quantizedStartStep
     :quantizedEndStep quantizedEndStep
     :instrument (:instrument note 0)}))

(defn phrase2noteseq [phrase]
  (let [stepsPerQuarter (->> phrase
                             (map #(/ 1 (:duration %)))
                             (reduce lcm))]
    {:quantizationInfo {:stepsPerQuarter stepsPerQuarter}
     :notes (map #(phrase2noteseq-note % stepsPerQuarter) phrase)}))

(defn magenta2leipzig-note
  [note, stepsPerQuarter]
  (let [time     (-> note :quantizedStartStep
                     Integer/parseInt
                     (/ stepsPerQuarter ))
        duration (-> note :quantizedEndStep
                     Integer/parseInt
                     (/ stepsPerQuarter )
                     (- time))]
    (hash-map :pitch (:pitch note)
              :time time
              :duration duration
              :instrument (:instrument note))))


(defn magenta2leipzig [note_sequence]
  (let [notes (:notes note_sequence)
        stepsPerQuarter (-> note_sequence
                            :quantizationInfo
                            :stepsPerQuarter)]
    (map #(magenta2leipzig-note % stepsPerQuarter) notes)))


(defn coconet [seed-phrase]
  (let [note_sequence (->> seed-phrase
                           (filter #(contains? % :pitch))
                           (phrase2noteseq))
        stepsPerQuarter (-> note_sequence
                            :quantizationInfo
                            :stepsPerQuarter)]
       (->> (client/post "http://localhost:3000/coconet"
                         {:accept :json
                          :as :jsoncontent-type
                          :body  (json/write-str
                                  (stringify-keys
                                   {:note_sequence note_sequence}))
                          :content-type :json})
            (:body)
            (json/read-str)
            (keywordize-keys)
            (#(assoc-in % [ :quantizationInfo :stepsPerQuarter] stepsPerQuarter))
            (magenta2leipzig))))
