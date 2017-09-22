//
//  PollenPushHandler.swift
//  PollenSDK
//
//  Created by Timothy Barraclough on 22/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation

public protocol PollenPushHandler {

    func registerForRemoteNotifications(in application: RemoteNotificationConfigurableProtocol)

    func didRegisterForRemoteNotifications(deviceToken: Data)

    func didFailToRegisterForRemoteNotifications(error: Error)

    func didReceiveRemoteNotification(notification: [AnyHashable : Any])
}
