//
//  Result.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

public enum Result<A> {
    case success(A)
    case error(Error)
}
