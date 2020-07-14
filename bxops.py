'''
@sonicskye@2020

bx operations
Adapted from uBX.pas from BitAnonymizer (2016)

'''
import fire
from shellops import getbxoutput
from utilities import quotedstr, singlequotedstr
import json

#################################### encoding ####################################

def base16encode(s=""):
    command = "base16-encode " + quotedstr(s)
    # same result as the following
    # source: https://python-forum.io/Thread-how-to-convert-a-string-to-hex
    # print(s.encode('utf-8').hex())
    return getbxoutput(command)


def bitcoin160(s=""):
    command = "bitcoin160 " + base16encode(s)
    return getbxoutput(command)


def scriptencode(script=""):
    command = "script-encode " + quotedstr(script)
    return getbxoutput(command)


def encodemsgopreturn(msg=""):
    msghex = base16encode(msg)
    msgopreturn = "return [" + msghex + "]"
    msgopreturnencoded = scriptencode(msgopreturn)
    return msgopreturnencoded


def addresstoscript(address=""):
    script = ""
    command = "address-decode -f json " + quotedstr(address)
    raw = getbxoutput(command)
    if raw != "":
        res = json.loads(raw)
        payload = res['wrapper']['payload']
        pubkeyhash160 = bitcoin160(payload)
        script = 'dup hash160 [' + pubkeyhash160 + '] equalverify checksig'

    return script


def swapbytes(b=""):
    # https://stackoverflow.com/a/46111074
    #ba = bytearray.fromhex(b)
    #print(ba)
    #return b.reverse()
    blen = len(b)
    if blen % 2 > 0:
        b = "0" + b
    idx = blen
    strb = ""
    #print(b[1:2])
    for i in range(0, blen // 2):
        idx = idx - 2
        # https://www.freecodecamp.org/news/how-to-substring-a-string-in-python/
        buffer = b[idx:idx+2]
        strb = strb + buffer

    return strb


#################################### key management ####################################

def getnewseed():
    command = "seed"
    return getbxoutput(command)


def gethdprivkeyfromseed(seed=""):
    command = "hd-new " + quotedstr(seed)
    return getbxoutput(command)


def getecprivkeyfromseed(seed=""):
    command = "ec-new " + quotedstr(seed)
    return getbxoutput(command)


def getnewhdprivkeyfromprivkeyidx(privkey="", idx=0):
    command = "hd-private --index " + str(idx) + " " + quotedstr(privkey)
    return getbxoutput(command)


def getnewhdpubkeyfrompubkeyidx(pubkey="", idx=0):
    command = "hd-public --index " + str(idx) + " " + quotedstr(pubkey)
    return getbxoutput(command)


def gethdpubkeyfromprivkey(privkey=""):
    command = "hd-to-public " + quotedstr(privkey)
    return getbxoutput(command)


def hdtoec(key=""):
    command = "hd-to-ec " + quotedstr(key)
    return getbxoutput(command)


def getaddrfromeckey(key=""):
    command = "ec-to-address " + quotedstr(key)
    return getbxoutput(command)


def getnewkeys(seed="", idx=None):
    if seed == "":
        seed = getnewseed()
    # if no idx is supplied, then the hdprivkey is the master key
    if idx is not None:
        hdprivkey = getnewhdprivkeyfromprivkeyidx(gethdprivkeyfromseed(seed), idx)
    else:
        hdprivkey = gethdprivkeyfromseed(seed)
    ecprivkey = getecprivkeyfromseed(seed)
    hdpubkey = gethdpubkeyfromprivkey(hdprivkey)
    ecpubkey = hdtoec(hdpubkey)
    address = getaddrfromeckey(ecpubkey)

    return seed, hdprivkey, ecprivkey, hdpubkey, ecpubkey, address


#################################### network ####################################

def fetchbalance(address=""):
    balance = 0
    command = "fetch-balance -f json " + quotedstr(address)
    raw = getbxoutput(command)
    if raw != "":
        res = json.loads(raw)
        if 'received' in res['balance'] and 'spent' in res['balance']:
            balance = int(res['balance']['received']) - int(res['balance']['spent'])
    return balance


# get UTXOs with a minimum value of sat
def fetchutxo(address="", sat=0):
    command = "fetch-utxo -f json " + str(sat) + " " + quotedstr(address)
    raw = getbxoutput(command)
    if raw != "":
        res = json.loads(getbxoutput(command))
    else:
        res = ""
    return res['points']


#################################### fees ####################################

def txfee(input=1, output=1, extra=0, satbyte=20):
    # set a minimum tx fee to 10000
    # however, I think it should be calculated as satoshi/byte
    # now it is 1 satoshi/byte
    # https://en.bitcoin.it/wiki/Miner_fees
    # mintxfee = 10000

    # estimated transaction size
    txsize = 148 * input + 34 * output + 10 + extra
    fee = txsize * satbyte

    return fee


#################################### transactions ####################################


def txopreturn(senderaddress="", feeaddress="", changeaddress="", msg=""):
    # for safety, assume there would be 2 inputs in the transactions
    # we can fix this later if we want
    # we need 3 outputs: fee collection address, change address, and op_return
    # but we only compute 2 normal outputs, plus a special op_return message length computed separately
    msgopreturn = encodemsgopreturn(msg)
    lenmsgopreturn = len(msgopreturn)
    fee = txfee(2, 2, lenmsgopreturn, 20)
    # check the balance of the sender's address
    balance = fetchbalance(senderaddress)
    if balance >= fee:
        utxos = fetchutxo(senderaddress, fee)



def main():
    fire.Fire()
    #pass


if __name__ == "__main__":
    main()