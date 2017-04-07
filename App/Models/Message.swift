import Foundation
import Vapor
import Fluent

struct Message: Model {
    var id: Node?
    var content: String
    var username: String
    var createdDate: Date?
    var exists: Bool = false

    enum Error: Swift.Error {
        case dateNotSupported
    }

    init(username: String, content: String, createdDate: Date? = nil) {
        self.username = username
        self.content = content
        self.createdDate = createdDate
    }

    init(node: Node, in _: Context) throws {
        id = try node.extract("id")
        username = try node.extract("username")
        content = try node.extract("content")

        if let raw = node["created"]?.string {
            guard let date = dateFormatter.date(from: raw) else {
                throw Error.dateNotSupported
            }

            createdDate = date
        }
    }

    mutating func persist() throws {
        if id == nil {
            createdDate = Date()
        }
        try save()
    }

    func makeNode(context _: Context) throws -> Node {
        let dateString = createdDate.map { created in dateFormatter.string(from: created) }
        return try Node(node: [
            "id": id,
            "username": username,
            "content": content,
            "created": dateString,
        ])
    }

    static func prepare(_ database: Database) throws {
        try database.create("messages") { messages in
            messages.id()
            messages.string("username")
            messages.string("content")
            messages.datetime("created", optional: true)
        }
    }

    static func revert(_ database: Database) throws {
        try database.delete("messages")
    }
}

private var _df: DateFormatter?
private var dateFormatter: DateFormatter {
    if let df = _df {
        return df
    }

    let df = DateFormatter()
    df.locale = Locale(identifier: "en_US_POSIX")
    df.dateFormat = "yyyy-MM-dd HH:mm:ss"
    df.timeZone = TimeZone(secondsFromGMT: 0)
    _df = df
    return df
}
