(ns aisong.instruments
 (:require [clojure.repl :refer :all]
            [overtone.live  :refer :all]
            [overtone.inst.sampled-piano :refer :all]
            [aisong.utils :refer [defdrum-freesound]]
            [sonic-pi.basic :as basic]
            [sonic-pi.atmos :as atmos]
            [sonic-pi.traditional :as traditional]
            [sonic-pi.bell :as bell]
            [sonic-pi.tech :as tech]
            [sonic-pi.noise :as noise]
            [sonic-pi.retro :as retro]))

(swap! live-config assoc-in [:sc-args :max-buffers] 1024)

;;;;;;;;;;;;;;;;; Sonic Pi  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sonic-pi-names {"basic" ["fm"    "mod_fm"
                              "saw"   "mod_saw"
                              "dsaw"  "mod_dsaw"
                              "beep"  "mod_sine"
                              "subpulse" "square"
                              "tri"  "dtri"
                              "pulse" "mod_pulse"
                              "dpulse"]
                     "atmos" ["hollow" "growl" "dark_ambience"]
                     "traditional" ["pluck" "blade"]
                     "bell"  ["dull_bell" "pretty_bell"]
                     "tech"  ["tech_saws"]
                     "noise" ["noise" "bnoise" "pnoise" "gnoise" "cnoise"]
                     "retro" ["tb303" "hoover" "supersaw" "zawa" "prophet"]})

(doseq [[instrument-class names] (map identity sonic-pi-names)]
  (doseq [n names]
    (intern *ns* (symbol (str instrument-class "-" n))
            (resolve (symbol  (str instrument-class "/sonic-pi-" n ))))))

(def piano sampled-piano)
;;;;;;;;;;;;;;;;;; Drumkit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdrum-freesound snare      38934 :sustain 0 :level 3)
(defdrum-freesound kick       183097 :level 2)
(defdrum-freesound close-hat  802)
(defdrum-freesound open-snare 16309 :level 2)
(defdrum-freesound open-hat   26657)
(defdrum-freesound clap       48310)
(defdrum-freesound boom       44293)
(defdrum-freesound snap       87731)

(kick)
(open-snare)

(snare :sustain 0.2 :decay 1 :level 9)
;;;;;;;;;;;;;;;;;; Others ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst saw-wave [freq 440 attack 0.01 sustain 0.2 release 0.1 vol 0.4]
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(definst pick [note 60]
  (let [freq (midicps note)
        src (+ (pluck (* (pink-noise) (env-gen (perc 0.001 0.5) :action FREE)) 1 1 (/ 4 freq))
               (lf-tri (* freq 0.025))
               (sin-osc (/ freq 2))
               (sin-osc (/ freq 3)))
        filt (env-gen (perc 0.001 2) :action FREE)]
    (lpf (* 0.5 (* src filt)) 1250)))

(defn saw2 [music-note]
  (saw-wave (midi->hz (note music-note))))
