package io

import java.io.Closeable


trait Output[T] extends (T => Unit) with Closeable
