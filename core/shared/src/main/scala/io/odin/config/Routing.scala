package io.odin.config

import cats.Monad
import cats.effect.Clock
import io.odin.Level
import io.odin.Logger
import io.odin.LoggerMessage
import io.odin.loggers.DefaultLogger
import cats.instances.list._
import cats.syntax.all._

trait Routing {

  /**
    * Route logs to specific logger based on the fully qualified package name.
    * Beware of O(n) complexity due to the partial matching done during the logging
    */
  def enclosureRouting[F[_]: Clock: Monad](router: (String, Logger[F])*): DefaultBuilder[F] = {
    new DefaultBuilder[F](new EnclosureRouting(_, router.toList))
  }

  /**
    * Route logs to specific logger based on `Class[_]` instance.
    * Beware of O(n) complexity due to the partial matching done during the logging
    */
  def classRouting[F[_]: Clock: Monad](
      router: (Class[_], Logger[F])*
  ): DefaultBuilder[F] =
    new DefaultBuilder[F](new EnclosureRouting(_, router.toList.map {
      case (cls, logger) => cls.getName -> logger
    }))

  /**
    * Route logs based on their level
    *
    * Complexity should be roughly constant
    */
  def levelRouting[F[_]: Clock: Monad](router: Map[Level, Logger[F]]): DefaultBuilder[F] =
    new DefaultBuilder[F]({ default: Logger[F] =>
      new DefaultLogger[F](Level.Trace) {
        def submit(msg: LoggerMessage): F[Unit] = router.getOrElse(msg.level, default).log(msg)

        override def submit(msgs: List[LoggerMessage]): F[Unit] = {
          msgs.groupBy(_.level).toList.traverse_ {
            case (level, msgs) => router.getOrElse(level, default).log(msgs)
          }
        }

        def withMinimalLevel(level: Level): Logger[F] =
          levelRouting(router.map {
            case (level, logger) => level -> logger.withMinimalLevel(level)
          }).withDefault(default.withMinimalLevel(level))
      }
    })

}
