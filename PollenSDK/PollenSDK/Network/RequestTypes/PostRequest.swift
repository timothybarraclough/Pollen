//
//  PostRequests.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

public enum PostRequest : Encodable {

    case device(String)
    case message(uuid: String, message: String)

    static var encoder = JSONEncoder()

}

extension PostRequest {

    public func encode(to encoder: Encoder) throws {
        switch self {
        case .device(let uuid):

            struct Device : Codable {
                var uuid : String
            }

            try Device(uuid: uuid).encode(to: encoder)

        case .message(let uuid, let message) :

            struct Message: Codable {
                var uuid : String
                var message : String
            }

            try Message(uuid: uuid, message: message).encode(to: encoder)
        }
    }

    func path() -> String {
        switch self {
        case .device(_):
            return "devices"
        case .message(_ , _):
            return "messages"
        }
    }
}
