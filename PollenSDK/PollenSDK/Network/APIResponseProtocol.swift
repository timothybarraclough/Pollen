//
//  APIResponseProtocol.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

protocol APIResponder {
    func taskResponder(with responder: @escaping (Result<Data>) -> Void) -> (_ data:Data?, _ response:URLResponse?, _ error:Error?) -> Void
}
