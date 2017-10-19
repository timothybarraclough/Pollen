//
//  ViewController.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 21/09/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import UIKit
import PollenSDK

class ViewController: UIViewController, Style, Interruptible {

    @IBOutlet weak var testNotificationButton: UIButton!

    
    @IBAction func didPressNotificationButton(_ sender: Any) {

        sendTestNotification()
    }

    @IBOutlet weak var hatButton: UIButton!

    @IBOutlet weak var snareButton: UIButton!

    @IBAction func snareAction(_ sender: Any) {
        if let token = PollenNetworkSession.shared.deviceTokenString {
            PollenNetworkSession.shared.request(.sound(uuid: token, sound: "sn.wav")) { _ in

            }
        }

    }

    @IBAction func hatAction(_ sender: Any) {
        if let token = PollenNetworkSession.shared.deviceTokenString {
            PollenNetworkSession.shared.request(.sound(uuid: token, sound: "hat.wav") ) { _ in

            }
        }
    }
    func sendTestNotification() {

        if let token = PollenNetworkSession.shared.deviceTokenString {
            PollenNetworkSession.shared.request(.sound(uuid: token, sound: "kick.wav")) { _ in

            }
        }
    }

    override func viewDidLoad() {
        super.viewDidLoad()

        view.backgroundColor = colors.backgroundColor
        testNotificationButton.setTitle("kick notification", for: .normal)
        hatButton.setTitle("hat notification", for: .normal)
        snareButton.setTitle("snare notification", for: .normal)

        [testNotificationButton, hatButton, snareButton].forEach(applyButtonStyle)

    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
}

protocol Interruptible {
    func showAlert(in controller: UIViewController)
}

extension Interruptible {

    func showAlert(in controller: UIViewController) {
        let alert = UIAlertController(title: "Received remote notification",
                                      message: nil,
                                      preferredStyle: .alert)
            alert.addAction(UIAlertAction.init(title: "Dismiss",
                                               style: .cancel,
                                               handler: { action in
                                                alert.dismiss(animated: true, completion: nil)

            }))

        controller.present(alert, animated: true, completion: nil)
    }
}

