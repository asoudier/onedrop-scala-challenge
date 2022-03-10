package controllers

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import javax.inject._
import play.api._
import play.api.mvc._

import scala.concurrent.{Await, Future}
import play.api.libs.json.{ JsValue, Json}
import play.api.libs.ws
import scalaj.http.{Http, HttpResponse}
import play.api.cache.{AsyncCacheApi}
import play.api.libs.json.Format.GenericFormat
import scala.concurrent.duration._
import javax.inject.Inject

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(val controllerComponents: ControllerComponents, cache: AsyncCacheApi) extends BaseController {
  var api_key = "0702271ca53a655f2cf7f52444400830"
  val location_url = (city: String) =>  s"http://api.openweathermap.org/geo/1.0/direct?q=${city}&limit=1&appid=${api_key}"
  val zip_url = (zip: String) => s"http://api.openweathermap.org/geo/1.0/zip?zip=${zip}&appid=${api_key}"

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    Ok(views.html.index())
  }

  def saveToCache(city: Option[String], zip: Option[String], endpoint: String, params: String) = {
    cache.getOrElseUpdate[JsValue](city.mkString("") + zip.mkString("") + endpoint + params,  10.seconds) {
      val geo = if(zip.exists(_.trim.nonEmpty)) {
        zip_url(zip.mkString(""))
      } else if(city.exists(_.trim.nonEmpty)) {
        location_url(city.mkString(""))
      } else {
        throw new RuntimeException("Please provide either a city or zip code")
      }
      val location_response = Http(geo)
        .asString.body
      val parsed = Json.parse(location_response)
      val lat = (parsed \\ "lat").mkString("")
      val lon = (parsed \\ "lon").mkString("")

      val api_url = s"https://api.openweathermap.org/data/2.5/${endpoint}?lat=${lat}&lon=${lon}&${params}&appid=${api_key}"
      val weather_response = Json.parse(Http(api_url)
        .asString.body)
      Future.successful(weather_response)
    }
  }

  def getCurrentWeather(city: Option[String], zip: Option[String])  = Action {
    val endpoint = "onecall"
    val params = "exclude=hourly,minutely,daily"
    saveToCache(city, zip, endpoint, params)
    val cached_result = Await.result(cache.get(city.mkString("") + zip.mkString("") + endpoint + params), 10.seconds).mkString("")
    Ok(Json.parse(cached_result))
  }

  def getWeatherForecast(city: Option[String], zip: Option[String])  = Action { implicit request: Request[AnyContent] =>
    val endpoint = "onecall"
    val params = "exclude=hourly,minutely,current"
    saveToCache(city, zip, endpoint, params)
    val cached_result = Await.result(cache.get(city.mkString("") + zip.mkString("") + endpoint + params), 10.seconds).mkString("")
    Ok(Json.parse(cached_result))
  }

  def getDateWeatherForecast(city: Option[String], zip: Option[String], from: String, to: String)  = Action { implicit request: Request[AnyContent] =>
    val fromUnix = (DateTime.parse(from, DateTimeFormat.forPattern("yyyy-MM-dd").withZoneUTC()).getMillis) / 1000
    val toUnix =  (DateTime.parse(to, DateTimeFormat.forPattern("yyyy-MM-dd").withZoneUTC()).getMillis) / 1000
    val endpoint = "history/city"
    val params = s"type=hour&start=${fromUnix}&end=${toUnix}"
    saveToCache(city, zip, endpoint, params)
    val cached_result = Await.result(cache.get(city.mkString("") + zip.mkString("") + endpoint + params), 10.seconds).mkString("")
    Ok(Json.parse(cached_result))
  }

  def getFiveDayForecast(city: Option[String], zip: Option[String], date: String)  = Action { implicit request: Request[AnyContent] =>
    val dateUnix = (DateTime.parse(date, DateTimeFormat.forPattern("yyyy-MM-dd").withZoneUTC()).getMillis) / 1000
    val endpoint = "onecall/timemachine"
    val params = s"dt=${dateUnix}"
    saveToCache(city, zip, endpoint, params)
    val cached_result = Await.result(cache.get(city.mkString("") + zip.mkString("") + endpoint + params), 10.seconds).mkString("")
    val parsed = Json.parse(cached_result)
    val temps = (parsed \\ "temp")
    val tempLength = temps.length
    val tempsList = temps.mkString("/").split("/").toList.map(_.toDouble)
    val avg = tempsList.sum / tempLength
    val min = tempsList.reduceLeft(_ min _)
    val max = tempsList.reduceLeft(_ max _)
    val newPayload = Json.obj(
      "lat" -> (parsed \\ "lat").mkString(""),
      "lon" -> (parsed \\ "lon").mkString(""),
      "timezone" -> (parsed \\ "timezone").mkString(""),
      "average" -> avg,
      "min" -> min,
      "max" -> max
    )
    Ok(newPayload)
  }
}