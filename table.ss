(
 ("UDP server"
  "frontend"
  "mixer"
  "index"
  "pCTR server"
  "pCTR training"
  "pCTR expr platform"
  "user server"
  "realtime user server"
  "user pipeline"
  "page server"
  "updater"
  "site_updater"
  "bigtable"
  "user agent"
  "interest server"
  "introspector server"
  "log server"
  "click server"
  "js log server"
  "accounter"
  "LDA"
  "classifiers"
  "keyword extractor"
  "text miner"
  "touchstone"
  "auto import blob"
  "auction log"
  "query log"
  "click log")
 (("广告投放" "targeting" "index" "mixer" "frontend")
  ("  广告bidding" "bidding" "mixer")
  ("  广告位和虚拟广告位" "virtual_place" "frontend" "mixer")
  ("广告请求" "request")
  ("检索条件构造" "query_build")
  ("广告组触发和过滤" "retrieval_and_filtering" "index" "mixer" "user server" "page server")
  ("  bigtable" "retrieval_bigtable")
  ("  index" "retrieval_index")
  ("  随机触发" "retrieval_random")
  ("  过滤" "filtering")
  ("广告组的排序和计费" "ranking_and_pricing" "pCTR server" "pCTR training")
  ("  创意选择" "creative_selection")
  ("  广告的ranking" "ranking")
  ("  广告去重" "deduplication")
  ("  广告的pricing" "pricing")
  ("广告补余" "complement")
  ("广告展现" "impression")
  ("广告点击" "click")
  ("消耗速度控制" "control_cost_rate")
  ("特定广告的promotion" "promotion")
  ("广告预览" "preview")
  ("大表相关逻辑" "bigtable")
  ("电商优化" "ebusiness_optimization")
  ("用用户信息触发和排序特定垂直领域广告" "vertical_ads_domain")
  ("exploration and exploitation" "exploration_and_exploitation")
  ("基于budget的展示控制逻辑" "impression_probability_by_budget")
  ("landing page的处理" "landing_page_processing")
  ("ip到地域的映射" "ip_to_geo_mapping")
  ("一定比例的流量上只出拍拍广告" "paipai_only_traffic")
  ("联调环境" "integrated_test_system")
  ("index白板" "index_whiteboard")
  ("预发布环境" "prelaunch_test_system")))
