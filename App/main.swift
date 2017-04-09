import Vapor
import VaporPostgreSQL
import Auth
import Turnstile
import TurnstileCrypto
import TurnstileWeb
import Foundation
import HTTP
import Cookies

let drop = Droplet(workDir: workDir)

drop.middleware.append(AuthMiddleware(user: User.self))
drop.preparations.append(Message.self)
drop.preparations.append(User.self)
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

    api.get("me") { request in
        if let userJson = try? JSON(node: request.user().makeNode()) {
            return userJson
        }

        return try JSON(node: [])
    }
}

if let clientID = drop.config["facebook", "clientID"]?.string,
    let clientSecret = drop.config["facebook", "clientSecret"]?.string {

    let facebook = Facebook(clientID: clientID, clientSecret: clientSecret)

    drop.get("login", "facebook") { request in
        let state = URandom().secureToken
        let response = Response(redirect: facebook.getLoginLink(redirectURL: "http://localhost:8080/login/facebook/consumer", state: state).absoluteString)
        response.cookies["OAuthState"] = state
        return response
    }

    drop.get("login", "facebook", "consumer") { request in
        guard let state = request.cookies["OAuthState"] else {
            return Response(redirect: "/login")
        }
        let account = try facebook.authenticate(authorizationCodeCallbackURL: request.uri.description, state: state) as! FacebookAccount
        try request.auth.login(account)
        return Response(redirect: "/")
    }
} else {
    drop.get("login", "facebook") { request in
        return "You need to configure Facebook Login first!"
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
