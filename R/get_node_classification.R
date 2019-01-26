#' get the classification for each node in a model
#'
#' @param x a soundDatabase
#' @param model an optional model id
#' @return a dataframe with the classification
#' @export
get_node_classification <- function(x, model) {
  UseMethod("get_node_classification", x)
}

#' @export
get_node_classification.soundDatabase <- function(x, model) {
  if (missing(model)) {
    get_node_classification(x = x@Connection)
  } else {
    get_node_classification(x = x@Connection, model = model)
  }
}

#' @importFrom pool poolCheckout poolReturn
#' @export
get_node_classification.Pool <- function(x, model) {
  conn <- poolCheckout(x)
  on.exit(poolReturn(conn))
}

#' @importFrom assertthat assert_that is.number
#' @importFrom RSQLite dbGetQuery dbQuoteLiteral
#' @importFrom stats aggregate
#' @export
get_node_classification.SQLiteConnection <- function(x, model) {
  if (missing(model)) {
    sql <-
      "WITH cte_weight AS (
        SELECT pr.node, p.class, SUM(1 / pr.distance) AS weight
        FROM pulse AS p
        INNER JOIN prediction AS pr ON p.id = pr.pulse
        WHERE p.class IS NOT NULL
        GROUP BY pr.node, p.class
      ),
      cte_total AS (
        SELECT node, SUM(weight) AS total_weight
        FROM cte_weight
        GROUP BY node
      )

      SELECT
        cw.node, cw.class, c.abbreviation,
        100 * cw.weight / ct.total_weight AS probability
      FROM cte_weight AS cw
      INNER JOIN cte_total AS ct ON cw.node = ct.node
      INNER JOIN class AS c ON cw.class = c.id
      ORDER BY probability DESC"
  } else {
    assert_that(is.number(model))
    sql <- sprintf(
      "WITH cte_weight AS (
        SELECT pr.node, p.class, SUM(1 / pr.distance) AS weight
        FROM pulse AS p
        INNER JOIN prediction AS pr ON p.id = pr.pulse
        INNER JOIN node AS n on pr.node = n.id
        WHERE p.class IS NOT NULL AND n.model = %s
        GROUP BY pr.node, p.class
      ),
      cte_total AS (
        SELECT node, SUM(weight) AS total_weight
        FROM cte_weight
        GROUP BY node
      )

      SELECT
        cw.node, cw.class, c.abbreviation,
        100 * cw.weight / ct.total_weight AS probability
      FROM cte_weight AS cw
      INNER JOIN cte_total AS ct ON cw.node = ct.node
      INNER JOIN class AS c ON cw.class = c.id
      ORDER BY probability DESC",
      dbQuoteLiteral(x, model)
    )
  }
  class <- dbGetQuery(x, sql)
  dominant <- class[!duplicated(class$node), c("node", "class")]
  class$text <- sprintf("%s (%.0f%%)", class$abbreviation, class$probability)
  text <- aggregate(text ~ node, class, paste, collapse = ", ")
  merge(dominant, text, by = "node")
}
