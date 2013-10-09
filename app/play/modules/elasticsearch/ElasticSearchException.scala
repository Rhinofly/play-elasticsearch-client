package play.modules.elasticsearch

case class ElasticSearchException(status: Int, message: String) extends RuntimeException(message: String)