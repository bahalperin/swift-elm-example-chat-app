import HTTP
import Fluent
import Turnstile
import TurnstileCrypto
import TurnstileWeb
import Auth

final class User: Auth.User {
    // Field for the Fluent ORM
    var exists: Bool = false

    static var entity = "app_users"

    // Database Fields
    var id: Node?
    var username: String
    var facebookID = ""

    /**
     Authenticates a set of credentials against the User.
     */
    static func authenticate(credentials: Credentials) throws -> Auth.User {
        var user: User?

        switch credentials {
        /**
         Fetches the user by session ID. Used by the Vapor session manager.
         */
        case let credentials as Identifier:
            user = try User.find(credentials.id)

        /**
         Fetches the user by Facebook ID. If the user doesn't exist, autoregisters it.
         */
        case let credentials as FacebookAccount:
            if let existing = try User.query().filter("facebook_id", credentials.uniqueID).first() {
                user = existing
            } else {
                user = try User.register(credentials: credentials) as? User
            }

        default:
            throw UnsupportedCredentialsError()
        }

        if let user = user {
            return user
        } else {
            throw IncorrectCredentialsError()
        }
    }

    /**
     Registers users for UsernamePassword, Facebook, or Google accounts.
     */
    static func register(credentials: Credentials) throws -> Auth.User {
        var newUser: User

        switch credentials {
        case let credentials as FacebookAccount:
            newUser = User(credentials: credentials)
        default:
            throw UnsupportedCredentialsError()
        }

        if try User.query().filter("username", newUser.username).first() == nil {
            try newUser.save()
            return newUser
        } else {
            throw AccountTakenError()
        }

    }

    init(credentials: FacebookAccount) {
        self.username = "fb" + credentials.uniqueID
        self.facebookID = credentials.uniqueID
    }

    /**
     Initializer for Fluent
     */
    init(node: Node, in context: Context) throws {
        id = node["id"]
        username = try node.extract("username")
        facebookID = try node.extract("facebook_id")
    }

    /**
     Serializer for Fluent
     */
    func makeNode(context: Context) throws -> Node {
        return try Node(node: [
            "id": id,
            "username": username,
            "facebook_id": facebookID
            ])
    }

    static func prepare(_ database: Database) throws {
        try database.create("app_users") { users in
            users.id()
            users.string("username")
            users.string("facebook_id")
        }
    }

    static func revert(_ database: Database) throws {
        try database.delete("app_users")
    }
}

extension Request {
    func user() throws -> User {
        guard let user = try auth.user() as? User else {
            throw "Invalid user type"
        }
        return user
    }
}

extension String: Error {}
