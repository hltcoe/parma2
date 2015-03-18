package edu.jhu.hlt.parma.util

trait Active {
	def active: Boolean
}

trait IsActive extends Active {
	override def active: Boolean = true
}

trait IsNotActive extends Active {
	override def active: Boolean = false
}

trait Activatable extends Active {
	private var actv = true
	override def active: Boolean = actv
	def activate { actv = true }
	def deactivate { actv = false }
}

