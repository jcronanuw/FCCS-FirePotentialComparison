#-------------------------------------------------------------------------------
# Name:        dotnetcheck.py
# Purpose:     Read windows registry to check if the .NET framework is recent
#               enough to support FFT
#
# Author:      kjells
#
# Created:     02/09/2014
#-------------------------------------------------------------------------------
import _winreg as reg

def is_good_4_dot_5():
    retval = False
    try:
        registry = reg.ConnectRegistry(None,reg.HKEY_LOCAL_MACHINE)
        key = reg.OpenKey(registry, r"Software\Microsoft\NET Framework Setup\NDP\v4\Full")
        val = reg.QueryValueEx(key, "Release")
        if val[0] >= 378389:
            retval = True
    except WindowsError:
        pass
    return retval

def is_good_4_dot_0():
    retval = False
    try:
        registry = reg.ConnectRegistry(None,reg.HKEY_LOCAL_MACHINE)
        
        key_client = reg.OpenKey(registry, r"Software\Microsoft\NET Framework Setup\NDP\v4\Client")
        key_full = reg.OpenKey(registry, r"Software\Microsoft\NET Framework Setup\NDP\v4\Full")
        
        for key in [key_client, key_full]:
            val = reg.QueryValueEx(key, "Install")
            if 1 == val[0]:
                retval = True
                break;
    except WindowsError:
        pass
    return retval

#-------------------------------------------------------------------------------
# Start...
#-------------------------------------------------------------------------------
check = is_good_4_dot_0()
if check:
    print('\nSuccess: your installed .NET Framework is sufficient to run FuelFireTools!\n')
else:
    print('\nFailure: FuelFireTools requires version 4.0 or higher of the .NET Framework.\n')
