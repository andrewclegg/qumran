(ns com.nervechannel.qumran
  (:gen-class)
  (:use [clojure.contrib.string :only [upper-case]])
  (:import (java.io FileWriter))
  (:import (java.util ArrayList))
  (:import (com.sun.syndication.feed.synd SyndFeedImpl SyndEntryImpl))
  (:import (com.sun.syndication.io SyndFeedOutput)))

(defn make-setter
  "Takes a keyword identifying a property, e.g. :title or :fullName, and returns the corresponding setter
method name, e.g. \"setTitle\" or \"setFullName\"."
  [propkey]
  (let [propname (name propkey)]
    (str "set"
	       (.toUpperCase (subs propname 0 1))
	       (subs propname 1))))

(defn set-all!
  "Sets a number of properties, identified by keys like :title or :feedType, on an object.
Works by converting the property names into setters like \"setTitle\" or \"setFeedType\"."
  [obj prop-map]
  (doseq [[k v] prop-map]
    (let [method-name (make-setter k)]
      (clojure.lang.Reflector/invokeInstanceMethod
        obj
        method-name
        (into-array Object [v]))))
   obj) ; TODO do we need obj here?

(defn to-list
  "Converts a sequence to an (untyped) ArrayList."
  [sqn]
  (let [lst (ArrayList.)]
    (doseq [v sqn]
      (.add lst v))))

(defn new-feed
  "Creates a new Rome syndication feed with the options provided (a map).
The map keys correspond to the SyndFeedImpl setters documented here:
http://repo1.maven.org/maven2/net/java/dev/rome/rome/1.0.0/rome-1.0.0-javadoc.jar
(but as camel-cased property names rather than setters, e.g. title not setTitle)."
  [options]
  (let [feed (SyndFeedImpl.)]
    (set-all! feed options)))

(defn new-entry
  "Creates a new Rome syndication entry with the options provided (a map).
The map keys correspond to the SyndEntryImpl setters documented here:
http://repo1.maven.org/maven2/net/java/dev/rome/rome/1.0.0/rome-1.0.0-javadoc.jar
(but as camel-cased property names rather than setters, e.g. title not setTitle)."
  [options]
  (let [entry (SyndEntryImpl.)]
    (set-all! entry options)))

(defn make-populated
  "Creates a new Rome syndication feed with the options provided, and populates
it from a sequence of entries."
  [options entries]
  (let [feed (new-feed options)
        entry-list (to-list entries)]
    (.setEntries feed entry-list)))

; TODO comments and tests for following functions

(defn to-file
  [filename feed]
  (with-open [writer (FileWriter. filename)]
    (.output (SyndFeedOutput.) [feed writer])))

