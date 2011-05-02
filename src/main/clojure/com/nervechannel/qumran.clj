(ns com.nervechannel.qumran
  (:gen-class)
  (:use [clojure.contrib.string :only [upper-case]])
  (:use [clojure.contrib.except :only [throw-if]])
  (:import (java.io FileWriter))
  (:import (java.util ArrayList))
  (:import (java.beans Introspector))
  (:import (com.sun.syndication.feed.synd SyndFeedImpl SyndEntryImpl SyndContent SyndContentImpl))
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
; TODO we can probably neaten up that rethrow with a macro

(defn set-all! [inst prop-map]
  "Sets a number of properties, identified by keys like :title or :feedType, on a bean instance,
via introspection."
  (doseq [[k v] prop-map]
    (set-property! inst k v))
  inst)

(defn to-list [sqn]
  "Converts a sequence to an (untyped) ArrayList."
  (let [lst (ArrayList.)]
    (doseq [v sqn]
      (.add lst v))
    lst))

(defn new-feed [props]
  "Creates a new Rome syndication feed with the properties provided (a map).
The map keys correspond to the SyndFeedImpl setters documented here:
http://repo1.maven.org/maven2/net/java/dev/rome/rome/1.0.0/rome-1.0.0-javadoc.jar
(but as camel-cased property names rather than setters, e.g. :title not setTitle)."
  (let [feed (SyndFeedImpl.)]
    (set-all! feed props)))

(defn pre-new-entry [props]
  "Pre-processes the property map for a new entry, turning :description into
a SyndContent if found."
  (if (and (contains? props :description) (not (instance? SyndContent (props :description))))
    (let [cont (doto (SyndContentImpl.) (.setValue (str (props :description))))]
      (assoc props :description cont))
    props))

(defn new-entry [props]
  "Creates a new Rome syndication entry with the properties provided (a map).
The map keys correspond to the SyndEntryImpl setters documented here:
http://repo1.maven.org/maven2/net/java/dev/rome/rome/1.0.0/rome-1.0.0-javadoc.jar
(but as camel-cased property names rather than setters, e.g. :title not setTitle).
If a :description property is supplied as a string, it will be automatically
converted into a SyndContent instance."
  (let [entry (SyndEntryImpl.)
        props2 (pre-new-entry props)]
    (set-all! entry props2)))

(defn build [props maps]
  "Creates a new Rome syndication feed with the properties provided, and populates
with new entries created from the sequence of entry property maps provided."
  (let [feed (new-feed props)
        entry-list (to-list (map new-entry maps))]
    (.setEntries feed entry-list)
    feed))

; TODO comments and tests for following functions

(defn to-file [filename feed]
  (with-open [writer (FileWriter. filename)]
    (.output (SyndFeedOutput.) [feed writer])))
