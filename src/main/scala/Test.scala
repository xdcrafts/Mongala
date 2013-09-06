import com.mongodb.casbah.Imports._
import play.api.libs.json._

/**
 *
 * v.dubs
 * Date: 05.09.13 15:09
 */
object MongoDb {
  val mongoClient = MongoClient("localhost", 27017)
  implicit val db = mongoClient("test")
}
trait MongoImplicits {
  implicit def jsToMongoObject(js: JsValue): MongoDBObject = {
    def jsToValue(js: JsValue): AnyRef = js match {
      case JsObject(_)        => jsToMongoObject(js)
      case JsString(string)   => string
      case JsNumber(num)      => num
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
  implicit def entityToMongoObject[T](entity: T)(implicit writes: Writes[T]): MongoDBObject =
    jsToMongoObject(writes.writes(entity))
  implicit def mongoObjectToJsValue(mongoObject: DBObject): JsValue = Json.parse(mongoObject.toString)
  implicit def mongoObjectToEntity[T](mongoObject: DBObject)(implicit reads: Reads[T]): Option[T] =
    reads.reads(mongoObjectToJsValue(mongoObject)).asOpt
}
abstract class MonDao[T <% Identifiable[T]](val db: MongoDB) extends MongoImplicits {
  implicit val reads:   Reads[T]
  implicit val writes:  Writes[T]
  println(s"Getting storage ${getClass.getSimpleName.replaceAll("[^a-zA-Z0-9]", "")}")
  private[this] val storage = db(getClass.getSimpleName.replaceAll("[^a-zA-Z0-9]", ""))
  def save(entity: T): Option[T] = {
    val dbObject: MongoDBObject = entity
    this.storage.save(dbObject)
    dbObject.getAs[ObjectId]("_id").map{_.toString}.map{entity.withId(_)}
  }
  def read(id: Option[String]): Option[T] =
    this.storage.findOneByID(id.map{new ObjectId(_)}).map{mongoObjectToEntity(_)}.flatten
  def update(entity: T): Option[T] = {
    this.storage.update(MongoDBObject("_id" -> entity.id.map{new ObjectId(_)}), entityToMongoObject(entity))
    read(entity.id)
  }
  def remove(id: Option[String]) = this.storage.remove(MongoDBObject("_id" -> id.map{new ObjectId(_)}))
  def find: List[T] = this.storage.find.toList.map{mongoObjectToEntity(_)}.flatten[T].toList
  def drop = this.storage.drop
}
trait Identifiable[T] {
  this: Identifiable[T] =>
  def id: Option[String]

  def withId(id: String): T = withOptId(Option(id))
  def withoutId(): T = withOptId(None)

  protected def withOptId(id: Option[String]): T
}
case class User(id: Option[String] = None, login: String, password: String) extends Identifiable[User] {
  protected def withOptId(id: Option[String]): User = copy(id = id)
}
object User {
  val userReads = Json.reads[User]
  val userWrites = Json.writes[User]
}
object MongoUsers extends MonDao[User](MongoDb.db) {
  val reads   = User.userReads
  val writes  = User.userWrites
}
object Test extends App {
  MongoUsers.drop
  val user = MongoUsers.save(User(None, "admin", "admin")).get
  println(s"Saved user: $user")
  val updatedUser = MongoUsers.update(user.copy(password = "pwd")).get
  println(s"Updated user: $updatedUser")
  println(s"users: ${MongoUsers.find}")
  MongoUsers.remove(user.id)
  println(s"users: ${MongoUsers.find}")
  println(s"users: ${MongoUsers.read(user.id)}")
}
