//
//  PollenNetwork+PollenPushHandler.swift
//  PollenSDK
//
//  Created by Timothy Barraclough on 22/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation
import UserNotifications

extension PollenNetworkSession : PollenPushHandler {

    public func didRegisterForRemoteNotifications(deviceToken: Data) {

        let tokenChars : [String] = deviceToken.map { String(format: "%02.2hhx", $0) }
        let tokenString = tokenChars.joined()
        print("Authenticated with : \(tokenString)")
        request(.device(tokenString)) { _ in

        }
    }

    public func didFailToRegisterForRemoteNotifications(error: Error) {
        dump(error)
    }

    public func didReceiveRemoteNotification(notification: [AnyHashable : Any]) {

    }

    public func registerForRemoteNotifications(in application: RemoteNotificationConfigurableProtocol) {

        if #available(iOS 10.0, *) {
            let center = UNUserNotificationCenter.current()
            center.requestAuthorization(options:[.badge, .alert, .sound]) { (granted, error) in
                if granted {
                    DispatchQueue.main.async {
                        application.registerForRemoteNotifications()
                    }
                }
            }

        } else {
            let settings = UIUserNotificationSettings(types: [.alert, .badge, .sound], categories: nil)
            application.registerUserNotificationSettings(settings)
            application.registerForRemoteNotifications()
        }

    }
}
