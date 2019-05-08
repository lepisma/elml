(require 'elml)

(describe "Attributes"
  (it "work"
    (expect (elml-format-attrs [:href "something" :style "background: black; color: white;"])
            :to-equal " href=\"something\" style=\"background: black; color: white;\"")))
