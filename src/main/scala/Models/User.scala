package Models

import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag
import scala.concurrent.Future

case class User(login: String, password: String, userId: Long = 0L)

class UserTable(tag: Tag) extends Table[User](tag, "users") {
  val userId = column[Long]("usr_id", O.PrimaryKey, O.AutoInc)
  val login = column[String]("login", O.Unique)
  val password = column[String]("password")

  def * = (login, password, userId) <> (User.apply _ tupled, User.unapply)
}

object UserTable {
  val table = TableQuery[UserTable]
}

class UserRepository(db: Database) {
  def create(user: User): Future[User] =
    db.run(UserTable.table returning UserTable.table += user)

  def update(user: User): Future[Int] =
    db.run(UserTable.table.filter(_.userId === user.userId).update(user))

  def delete(usrId: Long): Future[Int] =
    db.run(UserTable.table.filter(_.userId === usrId).delete)

  def getById(usrId: Long): Future[Option[User]] =
    db.run(UserTable.table.filter(_.userId === usrId).result.headOption)
}
