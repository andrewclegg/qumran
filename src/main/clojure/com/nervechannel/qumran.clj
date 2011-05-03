(ns com.nervechannel.qumran
  (:gen-class)
  (:use [clojure.contrib.string :only [upper-case split]])
  (:use [clojure.contrib.except :only [throw-if]])
  (:import (java.io File IOException))
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
      (catch IllegalArgumentException e
        (throw (IllegalArgumentException.
                 (str "Property " prop " rejected value <" value "> -- wrong type?") e))))))
; TODO we can probably neaten up that rethrow with a macro

(defn set-all! [inst prop-map]
  "Sets a number of properties, identified by keys like :title or :feedType, on a bean instance,
via introspection."
  (doseq [[k v] prop-map]
    (set-property! inst k v))
  inst)

(defn roll-file [filepath]
  "Renames a file to a new timestamped file in the same location, called
<basename>-<timestamp>.<ext>, where timestamp is a Unix epoch derived from
the file's last-modified stamp. Returns the new filepath. Throws if the
rename fails."
  (let
    [old-file (File. filepath)
     old-name (.getName old-file)
     components (vec (split #"\." old-name))
     timestamp (format "-%d" (/ (.lastModified old-file) 1000))
     new-basename (if (> (count components) 1)
                    (apply str (concat
                                 (interpose "." (pop components))
                                 (list timestamp "." (peek components))))
                    (apply str (old-name timestamp)))
     dir (.getParent old-file)
     new-file (File. dir new-basename)]
    (if (.renameTo old-file new-file)
      (.getCanonicalPath new-file)
      (throw (IOException. (format "Failed to rename %s to %s" filepath (.getCanonicalPath new-file)))))))

(defn to-list [sqn]
  "Copies a sequence into an (untyped) ArrayList."
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

(defn get-entry [feed i]
  "Gets the entry at the given index position from a feed."
  (.get (.getEntries feed) i))

(defn to-file! [filename feed]
  "Writes the feed out to a file with the given name, overwriting it if it already exists,
and returns the number of bytes written."
  (let [sfo (SyndFeedOutput.)
        file (File. filename)]
      (.output sfo feed file)
      (.length file)))

(defn rollover! [filename feed]
  "Writes the feed out to a file with the given name, preserving any existing file found there as follows.
The old file is first renamed to <basename>-<timestamp>.<ext>, and then a <link rel=\"prev\">...</link>
field is added to the new feed, pointing at the old file. Then the new feed is written to the original
filename. If no file exists at that location already, the new feed is just saved without modifications.
If the old file cannot be rolled over for some reason, an exception is thrown. If another file already
exists with the same timestamped name as is required for the rollover (unlikely), IT WILL BE OVERWRITTEN. 
On success, the function returns a vector of the number of bytes written to the new feed file, and the
new timestamped filename of the old file (or nil if not applicable)."
  (comment TODO))