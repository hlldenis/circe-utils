//> using dep "io.circe::circe-generic::0.14.6"
//> using dep "io.circe::circe-parser::0.14.6"

import io.circe._
import io.circe.parser._
import io.circe.syntax._

object Main {
  def linearizeJson(root: Json): Json = {

    def go(
        json: Json,
        parentKey: Option[String] = None
    ): Map[String, Json] = {
      json.fold(
        jsonNull = Map.empty[String, Json],
        jsonBoolean = b =>
          parentKey
            .map(pk => Map(pk -> Json.fromBoolean(b)))
            .getOrElse(Map.empty),
        jsonNumber = n =>
          parentKey
            .map(pk => Map(pk -> Json.fromJsonNumber(n)))
            .getOrElse(Map.empty),
        jsonString = s =>
          parentKey
            .map(pk => Map(pk -> Json.fromString(s)))
            .getOrElse(Map.empty),
        jsonArray = arr => {
          arr.zipWithIndex.flatMap { case (value, idx) =>
            val newKey =
              parentKey.map(pk => s"$pk.$idx").getOrElse(idx.toString)
            go(value, Some(newKey))
          }.toMap
        },
        jsonObject = obj => {
          obj.toMap.flatMap { case (key, value) =>
            val newKey = parentKey.map(pk => s"$pk.$key").getOrElse(key)
            go(value, Some(newKey))
          }
        }
      )
    }

    go(root).asJson
  }

  def delinearizeJson(json: Json): Json = {

    def constructPath(path: List[String], value: Json, current: Json): Json = {
      path match {
        case head :: Nil =>
          if (head.forall(_.isDigit)) {
            val idx = head.toInt
            current.asArray match {
              case Some(arr) =>
                val padded = (arr ++ List.fill(idx - arr.length + 1)(Json.Null))
                  .updated(idx, value)
                padded.asJson
              case None =>
                (List.fill(idx)(Json.Null) :+ value).asJson
            }
          } else {
            current.asObject match {
              case Some(obj) =>
                obj.add(head, value).asJson
              case None =>
                JsonObject(head -> value).asJson
            }
          }
        case head :: tail =>
          if (head.forall(_.isDigit)) {
            val idx = head.toInt
            val nextJson =
              current.asArray.flatMap(_.lift(idx)).getOrElse(Json.obj())
            val updatedJson = constructPath(tail, value, nextJson)
            current.asArray match {
              case Some(arr) =>
                val padded = (arr ++ List.fill(idx - arr.length + 1)(Json.Null))
                  .updated(idx, updatedJson)
                padded.asJson
              case None =>
                (List.fill(idx)(Json.Null) :+ updatedJson).asJson
            }
          } else {
            val nextJson =
              current.asObject.flatMap(_.apply(head)).getOrElse(Json.obj())
            val updatedJson = constructPath(tail, value, nextJson)
            current.asObject match {
              case Some(obj) =>
                obj.add(head, updatedJson).asJson
              case None =>
                JsonObject(head -> updatedJson).asJson
            }
          }
        case Nil => current
      }
    }

    json.asObject match {
      case Some(obj) =>
        obj.toList.foldLeft(Json.obj()) { case (jsonObj, (key, value)) =>
          val path = key.split("\\.").toList
          constructPath(path, value, jsonObj)
        }
      case None => json
    }
  }

  def main(args: Array[String]): Unit = {

    val nestedJson =
      """
        |{"a": {"b" : {}, "c": 1}}
     """.stripMargin

    val linearJson =
      """
        |{"a.0.b": true, "a.3.c": false}
     """.stripMargin

    val linearizedJson = linearizeJson(parse(nestedJson).getOrElse(Json.Null))

    println(linearizedJson.spaces2)

    val delinearizedJson = delinearizeJson(
      parse(linearJson).getOrElse(Json.Null)
    )

    println(delinearizedJson.spaces2)
  }

}
