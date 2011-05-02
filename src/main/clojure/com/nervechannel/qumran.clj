(ns com.nervechannel.qumran
  (:gen-class)
  (:use [clojure.contrib.string :only [upper-case]])
  (:use [clojure.contrib.except :only [throw-if]])
  (:import (java.io FileWriter))
  (:import (java.util ArrayList))
  (:import (java.beans Introspector))
  (:import (com.sun.syndication.feed.synd SyndFeedImpl SyndEntryImpl))
  (:import (com.sun.syndication.io SyndFeedOutput)))

(defn property-descriptor [inst prop-name]
  "Gets the proeprty descriptor for the specifed property of a bean instance."
  (first
    (filter #(= (name prop-name) (.getName %))
            (.getPropertyDescriptors (Introspector/getBeanInfo (class inst))))))

(defn set-property! [inst prop value]
  "Sets the specifed property of the bean instance via introspection."
  (let [pd (property-descriptor inst prop)]
    (throw-if (nil? pd) (str "No such property: " prop))
    (try
      (.invoke (.getWriteMethod pd) inst (into-array [value]))
      (catch IllegalArgumentException _
        (throw (IllegalArgumentException.
                 (str "Property " prop " value <" value "> has the wrong type" )))))))

(defn set-all! [inst prop-map]
  "Sets a number of properties, identified by keys like :title or :feedType, on a bean instance,
via introspection."
  (doseq [[k v] prop-map]
    (set-property! inst k v))
   inst) ; TODO do we need inst here?

(defn to-list [sqn]
  "Converts a sequence to an (untyped) ArrayList."
  (let [lst (ArrayList.)]
    (doseq [v sqn]
      (.add lst v))))

(defn new-feed [options]
  "Creates a new Rome syndication feed with the options provided (a map).
The map keys correspond to the SyndFeedImpl setters documented here:
http://repo1.maven.org/maven2/net/java/dev/rome/rome/1.0.0/rome-1.0.0-javadoc.jar
(but as camel-cased property names rather than setters, e.g. title not setTitle)."
  (let [feed (SyndFeedImpl.)]
    (set-all! feed options)))

(defn new-entry [options]
  "Creates a new Rome syndication entry with the options provided (a map).
The map keys correspond to the SyndEntryImpl setters documented here:
http://repo1.maven.org/maven2/net/java/dev/rome/rome/1.0.0/rome-1.0.0-javadoc.jar
(but as camel-cased property names rather than setters, e.g. title not setTitle)."
  (let [entry (SyndEntryImpl.)]
    (set-all! entry options)))

(defn make-populated [options entries]
  "Creates a new Rome syndication feed with the options provided, and populates
it from a sequence of entries."
  (let [feed (new-feed options)
        entry-list (to-list entries)]
    (.setEntries feed entry-list)))

; TODO comments and tests for following functions

(defn to-file [filename feed]
  (with-open [writer (FileWriter. filename)]
    (.output (SyndFeedOutput.) [feed writer])))

