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
drop.preparations.append(Channel.self)
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

    api.get("channels") { _ in
        let channels = try Channel.query().all()
        let json = try channels.makeJSON()

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

let chat = Chat()

drop.socket("chat", String.self) { _, ws, channel in
    var username: String?

    if chat.channels[channel] == nil {
        let c = try Channel.query().filter("name", channel).first()
        chat.channels[channel] = c
    }

    ws.onText = { ws, text in
        let json = try JSON(bytes: Array(text.utf8))

        if let u = json.object?["username"]?.string {
            username = u
            chat.channels[channel]?.connections[u] = ws
            try chat.bot(message: "\(u) has joined. 👋", channel: channel)
        }

        if let u = username, let m = json.object?["message"]?.string {
            try chat.send(name: u, message: m, channel: channel)
        }
    }

    ws.onClose = { _, _, _, _ in
        guard let u = username else {
            return
        }

        chat.channels[channel]?.connections.removeValue(forKey: u)
        try chat.bot(message: "\(u) has left", channel: channel)
    }
}

drop.run()

