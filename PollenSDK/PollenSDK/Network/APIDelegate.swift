//
//  APIDelegate.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

class APIDelegate : NSObject, APIResponder {

    func taskResponder(with responder: @escaping (Result<Data>) -> Void) -> (_ data:Data?, _ response:URLResponse?, _ error:Error?) -> Void {

        return { data, response, error in

            if let error = error {
                responder(.error(error))
                return
            }

            guard let data = data else {
                responder(.error(APIError.dataParsingError))
                return
            }
            if let resultString = String(data: data, encoding: .utf8) {
                print(resultString)
            }

            responder(.success(data))
        }
    }
}
