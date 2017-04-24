import Foundation
import Vapor

class Chat {
    var channels: [String: Channel]

    func bot(message: String, channel: String) throws {
        try send(name: "Bot", message: message, channel: channel)
    }

    func send(name: String, message: String, channel: String) throws {
        if let chan = channels[channel] {
            try chan.send(name: name, message: message)
        }
    }

    func addChannel(name: String) {
        channels[name] = Channel(name: name)
    }

    init() {
        channels = [:]
    }
}
