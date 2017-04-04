import Vapor
import Fluent

struct Message: Model {
    var id: Node?
    var content: String
	var username: String
    var exists: Bool = false

    init(username: String, content: String) {
        self.username = username
        self.content = content
    }

    init(node: Node, in context: Context) throws {
        id = try node.extract("id")
        username = try node.extract("username")
        content = try node.extract("content")
    }

    func makeNode(context: Context) throws -> Node {
        return try Node(node: [
            "id": id,
            "username": username,
            "content": content
        ])
    }

    static func prepare(_ database: Database) throws {
        try database.create("messages") { messages in
            messages.id()
            messages.string("username")
            messages.string("content")
        }
    }

    static func revert(_ database: Database) throws {
        try database.delete("messages")
    }
}
