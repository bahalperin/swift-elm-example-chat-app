import Foundation
import Vapor
import Fluent

struct Channel: Model {
    var id: Node?
    var name: String
    var createdDate: Date?
    var connections: [String: WebSocket]

    var exists: Bool = false

    enum Error: Swift.Error {
        case dateNotSupported
    }

    func bot(_ message: String) throws {
        try send(name: "Bot", message: message)
    }

    func send(name: String, message: String) throws {
        let message = message.truncated(to: 256)

        var msg = Message(username: name, content: message)
        try? msg.persist()
        let json = try msg.makeJSON()

        for (username, socket) in connections {
            guard username != name else {
                continue
            }

            try socket.send(json)
        }
    }

    init(name: String, createdDate: Date? = nil) {
        self.name = name
        self.createdDate = createdDate
        self.connections = [:]
    }

    init(node: Node, in _: Context) throws {
        id = try node.extract("id")
        name = try node.extract("name")

        if let raw = node["created"]?.string {
            guard let date = dateFormatter.date(from: raw) else {
                throw Error.dateNotSupported
            }

            createdDate = date
        }

        connections = [:]
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
            "name": name,
            "created": dateString
        ])
    }

    static func prepare(_ database: Database) throws {
        try database.create("channels") { channels in
            channels.id()
            channels.string("name")
            channels.custom("created", type: "TIMESTAMP", optional: true)
        }
    }

    static func revert(_ database: Database) throws {
        try database.delete("channels")
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
