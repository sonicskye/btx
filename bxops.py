'''
@sonicskye@2020

bx operations
Adapted from uBX.pas from BitAnonymizer (2016)

'''
import fire
from shellops import getbxoutput
from utilities import quotedstr, singlequotedstr
import json

#################################### general functions ####################################

# without any exception handling to handle the required numofbytes, be careful!
def inttohex(int=0,numofbytes=1):
    return (int).to_bytes(numofbytes, byteorder='big').hex()
    #return hex(int)[2:]

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
        #pubkeyhash160 = bitcoin160(payload)
        script = 'dup hash160 [' + payload + '] equalverify checksig'

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


def gettxid(rawtx):
    pass


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


def getecpubkeyfromecprivkey(ecprivkey=""):
    command = "ec-to-public " + quotedstr(ecprivkey)
    return getbxoutput(command)


def getaddressfromecpubkey(ecpubkey=""):
    command = "ec-to-address " + quotedstr(ecpubkey)
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

#################################### signing ####################################

# https://github.com/libbitcoin/libbitcoin-explorer/wiki/bx-input-sign
def inputsign(privkey, contract, trx, idx=0, signtype="all"):
    command = "input-sign " + "-i " + str(idx) + " -s " + signtype + " " + privkey + " " + quotedstr(contract) + " " + trx
    #print(command)
    return getbxoutput(command)


# https://github.com/libbitcoin/libbitcoin-explorer/wiki/bx-input-set
def inputset(signature, ecpubkey, trx, idx=0):
    endorsement = "[" + signature + "] " + "[" + ecpubkey + "]"
    command = "input-set " + "-i " + str(idx) + " " + quotedstr(endorsement) + " " + trx
    #print(command)
    #print()
    return getbxoutput(command)

#################################### network ####################################

def fetchheight():
    command = "fetch-height"
    return getbxoutput(command)


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
# dictionary of {hash, index, value}
def fetchutxo(address="", sat=0):
    command = "fetch-utxo -f json " + str(sat) + " " + quotedstr(address)
    #print(command)
    raw = getbxoutput(command)
    #print(raw)
    if raw != "":
        res = json.loads(getbxoutput(command))
    else:
        res = ""
    return res['points']


def validatetx(tx):
    command = "validate-tx " + tx
    raw = getbxoutput(command)
    if "valid" in raw:
        return True
    else:
        return False


def sendtx(tx):
    command = "send-tx " + tx
    return getbxoutput(command)


def fetchtx(txid):
    command = "fetch-tx -f json " + quotedstr(txid)
    raw = getbxoutput(command)
    if raw != "":
        res = json.loads(getbxoutput(command))
        ret = res['transaction']
    else:
        ret = ""
    return ret


def getutxoscript(txid, idx=0):
    txdata = fetchtx(txid)
    if 'outputs' in txdata:
        if len(txdata['outputs']) > idx:
            return txdata['outputs'][idx]['script']


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

def txcreate(senderecprivkey="", changeaddress="", msg=""):
    # for safety, assume there would be 2 inputs in the transactions
    # we can fix this later if we want
    # we need 2 outputs: change address, and op_return
    # but we only compute 1 normal outputs, plus a special op_return message length computed separately
    msgopreturn = encodemsgopreturn(msg)
    lenmsgopreturn = len(msgopreturn)
    fee = txfee(2, 1, lenmsgopreturn, 20)
    # check the balance of the sender's address
    senderaddress = getaddressfromecpubkey(getecpubkeyfromecprivkey(senderecprivkey))
    senderscript = addresstoscript(senderaddress)
    senderecpubkey = getecpubkeyfromecprivkey(senderecprivkey)
    balance = fetchbalance(senderaddress)
    if balance >= fee:
        utxos = fetchutxo(senderaddress, fee)

    #print(utxos)

    # construct txin
    txin = ""
    txinctx = len(utxos)
    txinnum = inttohex(len(utxos))
    txin = txin + txinnum
    txinamount = 0
    for utxo in utxos:
        hashswappedbytes = swapbytes(utxo["hash"])
        idxswappedbytes = swapbytes(inttohex(int(utxo["index"]), 4))
        txinamount = txinamount + int(utxo["value"])
        txin = txin + hashswappedbytes + idxswappedbytes
        # signature place
        txin = txin + "00"
        # sequence number
        txin = txin + "ffffffff"

    # construct txout
    # first txout: the message

    txout = ""
    txoutnum = inttohex(1)
    txout = txout + txoutnum

    #msgopreturn is the first output
    '''
    txoutnum = inttohex(2)
    # the value is zero in 8 bytes
    txoutvalue = inttohex(0, 8)
    msglen = inttohex(lenmsgopreturn // 2)
    #print(msgopreturn)
    txout = txout + txoutvalue + msglen + msgopreturn
    '''

    # second txout: change
    txoutvalue = swapbytes(inttohex(txinamount - fee, 8))
    txoutscriptencoded = scriptencode(addresstoscript(changeaddress))
    txoutlen = inttohex(len(txoutscriptencoded) // 2)
    locktime = "00000000"
    txout = txout + txoutvalue + txoutlen + txoutscriptencoded + locktime

    # general stuff
    protocolversion = "01000000"
    unsignedtx = protocolversion + txin + txout

    #print(unsignedtx)
    # signing stuff
    signedtx = unsignedtx
    for i in range(0, txinctx):
        #print(i)
        utxoscript = getutxoscript(utxos[i]['hash'], int(utxos[i]['index']))
        signature = inputsign(senderecprivkey, utxoscript, unsignedtx, i)
        #print(signature)
        signedtx = inputset(signature, senderecpubkey, signedtx, i)

    #print(signedtx)
    # validating tx
    if validatetx(signedtx):
        return signedtx
    else:
        return False

# TODO: check fetchutxo. The current function fetches unconfirmed utxo.
# TODO: check sendtx feature. It is currently disabled.
# TODO: try it out once again.
def txopreturn(senderecprivkey="", changeaddress="", msg=""):
    # for safety, assume there would be 2 inputs in the transactions
    # we can fix this later if we want
    # we need 2 outputs: change address, and op_return
    # but we only compute 1 normal outputs, plus a special op_return message length computed separately
    msgopreturn = encodemsgopreturn(msg)
    lenmsgopreturn = len(msgopreturn)
    fee = txfee(2, 1, lenmsgopreturn, 20)
    # check the balance of the sender's address
    senderaddress = getaddressfromecpubkey(getecpubkeyfromecprivkey(senderecprivkey))
    senderscript = addresstoscript(senderaddress)
    senderecpubkey = getecpubkeyfromecprivkey(senderecprivkey)
    balance = fetchbalance(senderaddress)
    if balance >= fee:
        utxos = fetchutxo(senderaddress, fee)

    # construct txin
    txin = ""
    txinctx = len(utxos)
    txinnum = inttohex(len(utxos))
    txin = txin + txinnum
    txinamount = 0
    for utxo in utxos:
        hashswappedbytes = swapbytes(utxo["hash"])
        idxswappedbytes = swapbytes(inttohex(int(utxo["index"]), 4))
        txinamount = txinamount + int(utxo["value"])
        txin = txin + hashswappedbytes + idxswappedbytes
        # signature place
        txin = txin + "00"
        # sequence number
        txin = txin + "ffffffff"

    # construct txout
    # first txout: the message
    txout = ""
    #msgopreturn is the first output
    txoutnum = inttohex(2)
    # the value is zero in 8 bytes
    txoutvalue = inttohex(0, 8)
    msglen = inttohex(lenmsgopreturn // 2)
    #print(msgopreturn)
    txout = txout + txoutnum + txoutvalue + msglen + msgopreturn

    # second txout: change
    txoutvalue = swapbytes(inttohex(txinamount - fee, 8))
    txoutscriptencoded = scriptencode(addresstoscript(changeaddress))
    txoutlen = inttohex(len(txoutscriptencoded) // 2)
    locktime = "00000000"
    txout = txout + txoutvalue + txoutlen + txoutscriptencoded + locktime

    # general stuff
    protocolversion = "01000000"
    unsignedtx = protocolversion + txin + txout

    #print(unsignedtx)
    # signing stuff
    signedtx = unsignedtx
    for i in range(0, txinctx):
        #print(i)
        utxoscript = getutxoscript(utxos[i]['hash'], int(utxos[i]['index']))
        signature = inputsign(senderecprivkey, utxoscript, unsignedtx, i)
        #print(signature)
        signedtx = inputset(signature, senderecpubkey, signedtx, i)

    #print("Signed tx: " + signedtx)
    # validating tx.
    if validatetx(signedtx):
        return signedtx
    else:
        return False

    #sendtx(signedtx)

    #print("Transaction sent")


def main():
    fire.Fire()
    #pass


if __name__ == "__main__":
    main()