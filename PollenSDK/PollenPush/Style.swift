//
//  Style.swift
//  PollenPush
//
//  Created by Timothy Barraclough on 19/10/17.
//  Copyright © 2017 FloraCreative. All rights reserved.
//

import Foundation
import UIKit

protocol Style {
    var colors: AppColorProtocol { get }
    func applyButtonStyle(_ button: UIButton)
}

extension Style {


    var cornerRadius : CGFloat { return 7.0 }

    var fontSize : CGFloat { return 24.0 }

    var colors : AppColorProtocol { return AppColor() }

    var buttonFont : String { return "Avenir-Black" }

    func applyButtonStyle(_ button: UIButton) {

        button.layer.cornerRadius = cornerRadius
        button.clipsToBounds = true
        button.backgroundColor = colors.confirmColor
        button.setTitleColor(colors.buttonTextColor, for: .normal)
        button.setTitleColor(colors.buttonTextColor, for: .highlighted)
        button.setTitleColor(colors.buttonTextColor, for: .selected)

        button.titleLabel?.textColor = colors.buttonTextColor
        button.titleLabel?.font = UIFont(name: buttonFont, size: fontSize)
    }
}

protocol AppColorProtocol {
    var backgroundColor : UIColor { get }
    var confirmColor : UIColor { get }
    var buttonTextColor : UIColor { get }

}

struct AppColor : AppColorProtocol {

    var backgroundColor: UIColor { return UIColor(red:0.91, green:1.00, blue:0.44, alpha:1.0) }

    //UIColor(red:1.00, green:0.84, blue:0.44, alpha:1.0)

    // UIColor(red:1.00, green:0.59, blue:0.44, alpha:1.0)

    // UIColor(red:1.00, green:0.44, blue:0.65, alpha:1.0)


    var confirmColor: UIColor { return UIColor(red:0.44, green:0.84, blue:1.00, alpha:1.0) }

    var buttonTextColor: UIColor { return .white }

}

