import Vapor
import VaporMySQL
import Foundation

let drop = Droplet(workDir: workDir)

drop.preparations.append(Message.self)
try drop.addProvider(VaporMySQL.Provider.self)


// MARK: Visit

drop.get { req in
    // Design from: http://codepen.io/supah/pen/jqOBqp?utm_source=bypeople
    return try drop.view.make("index.html")
}

drop.group("api") { api in
    api.get("messages") { request in
        let messages = try Message.query().all()
        let json = try messages.makeJSON()

        return json
    }
}

// MARK: Sockets

let room = Room()

drop.socket("chat") { req, ws in
    var username: String? = nil

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

    ws.onClose = { ws, _, _, _ in
        guard let u = username else {
            return
        }

        room.connections.removeValue(forKey: u)
        try room.bot("\(u) has left")
    }
}

drop.run()
