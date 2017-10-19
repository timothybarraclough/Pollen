//
//  ViewController.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import UIKit
import PollenSDK

class ViewController: UIViewController, Style {

    @IBOutlet weak var testNotificationButton: UIButton!

    
    @IBAction func didPressNotificationButton(_ sender: Any) {

        sendTestNotification()
    }

    func sendTestNotification() {

        if let token = PollenNetworkSession.shared.deviceTokenString {
            print("Sending notification to: \(token)")
            PollenNetworkSession.shared.request(.message(uuid: token, message: "Nothing here yet!")) { _ in

            }
        }
        //PollenNetworkSession.shared.deviceToken
        
    }
    override func viewDidLoad() {
        super.viewDidLoad()

        view.backgroundColor = colors.backgroundColor

        testNotificationButton.setTitle("send test notification", for: .normal)

        applyButtonStyle(testNotificationButton)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
}

