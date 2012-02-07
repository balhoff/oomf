package org.nescent.informatics.oomf

import java.io.File
import java.io.FilenameFilter

object DotFileFilter  extends FilenameFilter {

	def accept(dir: File, name: String): Boolean = {
			return !name.startsWith(".");
	}

} 