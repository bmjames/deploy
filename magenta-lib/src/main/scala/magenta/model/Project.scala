package magenta
package model

import tasks.Task
import java.io.File


case class Host(
    name: String,
    apps: Set[App] = Set.empty,
    stage: String = "NO_STAGE",
    connectAs: Option[String] = None)
{
  def app(name: String) = this.copy(apps = apps + App(name))
  def app(app: App) = this.copy(apps= apps + app)

  def as(user: String) = this.copy(connectAs = Some(user))

  // this allows "resin" @: Host("some-host")
  def @:(user: String) = as(user)

  lazy val connectStr = (connectAs map { _ + "@" } getOrElse "") + name
}





/*
 An action represents a step within a recipe. It isn't executable
 until it's resolved against a particular host.
 */
trait Action {
  def resolve(host: Host): List[Task]
  def apps: Set[App]
  def description: String
}





case class Recipe(
  name: String,
  actions: List[Action] = Nil,
  dependsOn: List[String] = Nil
)



case class Project(
  packages: Map[String, Package] = Map.empty,
  recipes: Map[String, Recipe] = Map.empty
) {
  lazy val applications = packages.values.flatMap(_.apps).toSet
}


case class Package(
  name: String,
  pkgApps: Set[App],
  pkgSpecificData: Map[String, String],
  pkgTypeName: String,
  srcDir: File) {

  def mkAction(name: String): Action = pkgType.mkAction(name)

  lazy val pkgType = pkgTypeName match {
    case "jetty-webapp" => new JettyWebappPackageType(this)
    case "resin-webapp" => new ResinWebappPackageType(this)
    case "file" => new FilePackageType(this)
    case "demo" => new DemoPackageType(this)
    case unknown => sys.error("Package type %s of package %s is unknown" format (unknown, name))
  }

  val data = pkgType.defaultData ++ pkgSpecificData

  val apps = pkgApps
}