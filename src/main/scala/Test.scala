import com.mongodb.casbah.Imports._
import play.api.libs.json._

/**
 *
 * v.dubs
 * Date: 05.09.13 15:09
 */
trait MongoDb {
  private val mongoClient = MongoClient("localhost", 27017)
  def connect: MongoDB = mongoClient("test")
  def close: Unit = mongoClient.close
}
object MongoDb extends MongoDb
trait MongoImplicits {
  import scala.util.parsing.combinator.RegexParsers
  object IdParser extends RegexParsers {
    private def dropLastSymbol(s: String) = s.substring(0, s.length - 1)
    def oid = ("(.*?)oid(.*?):(.*?)\"".r ~> "(.*?)\"".r) ^^ dropLastSymbol
  }
  implicit def entityToMongoObject[T](entity: T)(implicit writes: Writes[T]): MongoDBObject = {
    def jsToMongoObject(js: JsValue): MongoDBObject = {
      def jsToValue(js: JsValue): Any = js match {
        case JsObject(_)        => jsToMongoObject(js)
        case JsString(string)   => string
        case JsNumber(num)      => num.toDouble
        case JsBoolean(boolean) => boolean.toString
        case JsArray(arr)       => arr.map{jsToValue(_)}.toList
        case JsNull             => None
        case JsUndefined(u)     => u
      }
      js match {
        case JsObject(fields) => MongoDBObject(fields.map{f => (f._1, jsToValue(f._2))}.toList)
        case jsValue          => MongoDBObject("value" -> jsToValue(jsValue))
      }
    }
    jsToMongoObject(writes.writes(entity))
  }
  implicit def mongoObjectToEntity[T <% Identifiable[T]](mongoObject: DBObject)(reads: Reads[T]): Option[T] = {
    def mongoObjectToJsValue(mongoObject: DBObject): (Option[String], JsValue) = {
      println(mongoObject.toString)
      val preprocessed = mongoObject.toString.replaceAll("\"true\"", "true").replaceAll("\"false\"", "false")
      val id = IdParser.parse(IdParser.oid, preprocessed).getOrElse("")
      (if (id.isEmpty) None else Some(id), Json.parse(preprocessed))
    }
    val pair = mongoObjectToJsValue(mongoObject)
    reads.reads(pair._2).asEither match {
      case Left(error) => println(error); None
      case Right(entity) => Some(entity.withOptId(pair._1))
    }
  }
}
abstract class MongoStorage[T <% Identifiable[T]](val db: MongoDB) extends MongoImplicits {
  implicit val reads:   Reads[T]
  implicit val writes:  Writes[T]
  println(s"DB: $db")
  println(s"Getting storage ${getClass.getSimpleName.replaceAll("[^a-zA-Z0-9]", "")}")
  private[this] val storage = db(getClass.getSimpleName.replaceAll("[^a-zA-Z0-9]", ""))
  def save(entity: T): Option[T] = {
    val dbObject: MongoDBObject = entity
    this.storage.save(dbObject)
    dbObject.getAs[ObjectId]("_id").map{_.toString}.map{entity.withId(_)}
  }
  def read(id: String): Option[T] = {
    this.storage.findOneByID(new ObjectId(id)).map{mongoObjectToEntity(_)(reads)}.flatten
  }
  def update(entity: T): Option[T] = {
    this.storage.update(MongoDBObject("_id" -> entity.id.map{new ObjectId(_)}), entityToMongoObject(entity.withoutId()))
    entity.id.flatMap{read(_)}
  }
  def remove(id: String) = this.storage.remove(MongoDBObject("_id" -> new ObjectId(id)))
  def find: List[T] = this.storage.find.toList.map{mongoObjectToEntity(_)(reads)}.flatten[T].toList
  def drop = this.storage.drop
}
trait Identifiable[T] {
  this: Identifiable[T] =>
  def id: Option[String]

  def withId(id: String): T = withOptId(Option(id))
  def withoutId(): T = withOptId(None)

  def withOptId(id: Option[String]): T
}
case class Meta(text: String, q: Int, b: Boolean)
object Meta {
  implicit val metaReads = Json.reads[Meta]
  implicit val metaWrites = Json.writes[Meta]
}
case class User(userId: Option[String], login: String, password: String, meta: Meta) extends Identifiable[User] {
  def id = userId
  def withOptId(id: Option[String]): User = copy(userId = id)
}
object User {
  import Meta.metaReads
  import Meta.metaWrites
  implicit val userReads = Json.reads[User]
  implicit val userWrites = Json.writes[User]
}

object MongoUsers extends MongoStorage[User](MongoDb.connect) {
  val reads   = User.userReads
  val writes  = User.userWrites
}
object Test extends App {
  MongoUsers.drop
  val user = MongoUsers.save(User(None, "admin", "admin", Meta("text", 1, true))).get
  println(s"Saved user: $user")
  val updatedUser = MongoUsers.update(user.copy(password = "pwd", meta = Meta("text", 1, false))).get
  println(s"Updated user: $updatedUser")
  println(s"users: ${MongoUsers.find}")
  MongoUsers.remove(user.id.get)
  println(s"users: ${MongoUsers.find}")
  println(s"users: ${MongoUsers.read(user.id.get)}")
  MongoDb.close
}
