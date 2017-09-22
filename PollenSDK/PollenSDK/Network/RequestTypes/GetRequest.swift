//
//  GetRequest.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

public enum GetRequest {
    case devices

    func path() -> String {
        switch self {
        case .devices:
            return "devices"
        }
    }
}
