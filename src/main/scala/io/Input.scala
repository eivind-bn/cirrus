package io

import java.io.Closeable


trait Input[T] extends (() => T) with Closeable
