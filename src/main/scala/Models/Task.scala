package Models

import slick.lifted.Tag
import slick.jdbc.PostgresProfile.api._
import scala.concurrent.Future

case class Task(login: String, task: String, taskId: Long = 0L, finished: Boolean = false)

class TaskTable(tag: Tag) extends Table[Task](tag, "tasks") {
  val taskId = column[Long]("task_id", O.PrimaryKey, O.AutoInc)
  val login = column[String]("login")
  val task = column[String]("task")
  val finished = column[Boolean]("done")

  val taskFk = foreignKey("task_fk", login, TableQuery[UserTable])(_.login)

  def * = (login, task, taskId, finished) <> (Task.apply _ tupled, Task.unapply)
}

object TaskTable {
  val table = TableQuery[TaskTable]
}

class TaskRepository(db: Database) {
  def create(task: Task): Future[Task] =
    db.run(TaskTable.table returning TaskTable.table += task)

  def update(task: Task): Future[Int] =
    db.run(TaskTable.table.filter(_.taskId === task.taskId).update(task))

  def delete(taskId: Long): Future[Int] =
    db.run(TaskTable.table.filter(_.taskId === taskId).delete)

  def getById(taskId: Long): Future[Option[Task]] =
    db.run(TaskTable.table.filter(_.taskId === taskId).result.headOption)
}