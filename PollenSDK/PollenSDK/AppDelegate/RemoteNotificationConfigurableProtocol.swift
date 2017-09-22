//
//  RemoteNotificationConfigurableProtocol.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 22/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

public protocol RemoteNotificationConfigurableProtocol {
    func registerForRemoteNotifications()
    func registerUserNotificationSettings(_ settings: UIUserNotificationSettings)
}


