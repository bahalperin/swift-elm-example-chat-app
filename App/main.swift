import Vapor
import VaporPostgreSQL
import Foundation

let drop = Droplet(workDir: workDir)

drop.preparations.append(Message.self)
try drop.addProvider(VaporPostgreSQL.Provider.self)

// MARK: Visit

drop.get { _ in
    // Design from: http://codepen.io/supah/pen/jqOBqp?utm_source=bypeople
    return try drop.view.make("index.html")
}

drop.group("api") { api in
    api.get("messages") { _ in
        let messages = try Message.query().all()
        let json = try messages.makeJSON()

        return json
    }
}

// MARK: Sockets

let room = Room()

drop.socket("chat") { _, ws in
    var username: String?

    ws.onText = { ws, text in
        let json = try JSON(bytes: Array(text.utf8))

        if let u = json.object?["username"]?.string {
            username = u
            room.connections[u] = ws
            try room.bot("\(u) has joined. 👋")
        }

        if let u = username, let m = json.object?["message"]?.string {
            try room.send(name: u, message: m)
        }
    }

    ws.onClose = { _, _, _, _ in
        guard let u = username else {
            return
        }

        room.connections.removeValue(forKey: u)
        try room.bot("\(u) has left")
    }
}

drop.run()
