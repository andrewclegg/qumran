(ns com.nervechannel.test-qumran
  (:use clojure.test)
  (:use com.nervechannel.qumran)
  (:import (com.sun.syndication.feed.synd SyndFeedImpl)))

(deftest test-set-all
  (let [feed (set-all! (SyndFeedImpl.)
                               {:title "my title" :description "my description"})]
    (is (not (nil? feed)))
    (is (= "my title" (.getTitle feed)))
    (is (= "my description" (.getDescription feed)))))

(deftest test-to-list
  (let [lst (to-list [1 "a" \b])]
    (is (= 1 (.get lst 0)))
    (is (= "a" (.get lst 1)))
    (is (= \b (.get lst 2)))))

(deftest test-new-feed
  (let [feed (new-feed
               {:title "my title" :description "my description"})]
    (is (not (nil? feed)))
    (is (= "my title" (.getTitle feed)))
    (is (= "my description" (.getDescription feed)))))

(deftest test-new-entry
  (let [entry (new-entry
                {:title "my title" :link "http://example.org/" :description "my description"})]
    (is (not (nil? entry)))
    (is (= "my title" (.getTitle entry)))
    (is (= "http://example.org/" (.getLink entry)))
    (is (= "my description" (.. entry getDescription getValue)))))

(def *entries-data*
  [{:title "Some news" :description "Bin Laden wedding exclusive"}
   {:title "More news" :description "Kate Middleton killed by US special forces"}
   {:title "Commentary" :description "AV only fair way to decide Grand National winner"}])

(def *feed-data*
  {:title "Qumran News" :description "Plucking diced carrots of information from the data barf"})

(deftest test-build
  (let [feed (build *feed-data* *entries-data*)]
    (is (= "Qumran News" (.getTitle feed)))
    (is (= \P (first (.getDescription feed))))
    (is (= "Some news" (.getTitle (get-entry feed 0))))
    (is (= \B (first (.. (get-entry feed 0) getDescription getValue))))
    (is (= "More news" (.getTitle (get-entry feed 1))))
    (is (= \K (first (.. (get-entry feed 1) getDescription getValue))))
    (is (= "Commentary" (.getTitle (get-entry feed 2))))
    (is (= \A (first (.. (get-entry feed 2) getDescription getValue))))
    ))