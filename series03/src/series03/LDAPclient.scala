package series03

import java.util.Hashtable
import javax.naming._
import javax.naming.directory._
import javax.naming.ldap._

/* Université de Neuchâtel
 * Security
 * Assignment 3
 * Laura Rettig (laura.rettig@unifr.ch)
 * October 25, 2014
 */

object LDAPclient {
	
	def main(args: Array[String]) {
		// establish connection
		var env = new Hashtable[String, String]
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
		env.put(Context.PROVIDER_URL, "ldap://54.68.0.145")
		env.put(Context.SECURITY_AUTHENTICATION,"simple")
		env.put(Context.SECURITY_PRINCIPAL,"cn=admin,dc=security,dc=ch")
		env.put(Context.SECURITY_CREDENTIALS,"security20142014!")
		val ctx: Context = new InitialContext(env)
		val dirCtx: DirContext = new InitialDirContext(env)
		
		// display the contents of a given directory
		displayContents(ctx, dirCtx)
		
		// Add new entries in the LDAP server
		add(dirCtx, "Jane Doe", "Jane", null, "555-514 3333")
		displayContents(ctx, dirCtx)
		
		// Modify an entry
		modify(dirCtx, "Jane Doe", DirContext.ADD_ATTRIBUTE, "seeAlso", "cn=assaf,ou=students,dc=security,dc=ch")
		modify(dirCtx, "Jane Doe", DirContext.REPLACE_ATTRIBUTE, "telephoneNumber", "555-514 1234")
		modify(dirCtx, "Jane Doe", DirContext.REMOVE_ATTRIBUTE, "telephoneNumber")
		
		displayContents(ctx, dirCtx)
		
		// Delete an entry
		delete(ctx, "Jane Doe")
		displayContents(ctx, dirCtx)
		
		
		dirCtx.close
	}
	
	def displayContents(ctx: Context, dirCtx: DirContext, directory: String = "ou=students,dc=security,dc=ch") {
		println("Contents of directory " + directory + ":")
		val list = ctx.list(directory)
		while (list.hasMore) {
			val student = list.next
			println(student.getName)
			val attrs: Attributes = dirCtx.getAttributes(student.getNameInNamespace)
			val ae = attrs.getAll
			while (ae.hasMore) {
				val attr: Attribute = ae.next
				val values = attr.getAll 	// if there are multiple values for an attribute
				while (values.hasMore) println("\t" + attr.getID + ": " + values.next)
			}
		}
	}
	
	def add(dirCtx: DirContext, cn: String, sn: String, userPwd: String = null, telNum: String = null, seeAlso: String = null, desc: String = null) {
		val attrs: Attributes = new BasicAttributes(true)
		attrs.put("sn", sn)
		attrs.put("objectClass", "person")
		if (userPwd != null) attrs.put("userPassword", userPwd)
		if (telNum != null) attrs.put("telephoneNumber", telNum)
		if (seeAlso != null) attrs.put("seeAlso", seeAlso)
		if (desc != null) attrs.put("description", desc)
		dirCtx.bind("cn=" + cn + ",ou=students,dc=security,dc=ch", null, attrs)
		println("\nAdded " + cn + "\n")
	}
	
	def delete(ctx: Context, cn: String) {
		ctx.unbind("cn=" + cn + ",ou=students,dc=security,dc=ch")
		println("\nDeleted " + cn + "\n")
	}
	
	def modify(dirCtx: DirContext, cn: String, operation: Int, attr: String, newVal: String = null) {
		val attrs: Attributes = new BasicAttributes(true)
		attrs.put(attr, newVal)
		dirCtx.modifyAttributes("cn=" + cn + ",ou=students,dc=security,dc=ch", operation, attrs)
		println("\nModified attribute " + attr + " for student " + cn + "\n")
	}
}