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

(comment deftest test-to-list
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
                {:title "my title" :link "http://example.org/"})]
    (is (not (nil? entry)))
    (is (= "my title" (.getTitle entry)))
    (is (= "http://example.org/" (.getLink entry)))))