(require
  '[figwheel.main :as figwheel]
  '[dynadoc.core :as dynadoc])

(dynadoc/start {:port 5000
                :exclusions '#{odoyle.rules/parse}})

(figwheel/-main "--build" "docs")
