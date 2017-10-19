//
//  PollenNetworkSession.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

public class PollenNetworkSession {

    public static let shared : PollenNetworkSession = PollenNetworkSession()

    var hostDomain : URL! = URL(string:"http://localhost:8080")
    var delegate : APIResponder? = APIDelegate()

    public var deviceTokenString : String?

    public static func configure(_ network: PollenNetworkSession, withHostDomain hostDomain: URL ) {
        network.hostDomain = hostDomain
    }

    lazy public var session: URLSession = {
        let configuration = URLSessionConfiguration.default
        configuration.requestCachePolicy = .reloadIgnoringCacheData
        return URLSession(configuration: configuration)
    }()

    public func request(_ endpoint: GetRequest, with completion: @escaping (Result<Data>) -> Void) {
        let fullURL = hostDomain.appendingPathComponent(endpoint.path())
        if let completionHandler = delegate?.taskResponder(with: completion) {

            session
                .dataTask(with: fullURL, completionHandler: completionHandler)
                .resume()
        }
    }

    public func request(_ endpoint: PostRequest, with completion: @escaping (Result<Data>) -> Void) {

        let fullURL = hostDomain.appendingPathComponent(endpoint.path())
        if let completionHandler = delegate?.taskResponder(with: completion),
            let data = try? PostRequest.encoder.encode(endpoint){

            var pRequest = URLRequest(url: fullURL)
            pRequest.httpBody = data
            pRequest.httpMethod = "POST"
            pRequest.addValue("application/json", forHTTPHeaderField: "Content-Type")
            pRequest.addValue("application/json", forHTTPHeaderField: "Accept")

            session
                .dataTask(with: pRequest, completionHandler: completionHandler)
                .resume()
        }
    }
}
