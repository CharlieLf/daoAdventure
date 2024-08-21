import Result "mo:base/Result";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Option "mo:base/Option";
import Types "types";
actor {

    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let wallets = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);
    let token = {
        Name : Text = "CLT Token";
        Symbol : Text = "CEEL";
    };

    public query func tokenName() : async Text {
        return token.Name;
    };

    public query func tokenSymbol() : async Text {
        return token.Symbol;
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let ownerBalance = Option.get(wallets.get(owner), 0);
        wallets.put(owner, ownerBalance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let ownerBalance = Option.get(wallets.get(owner), 0);
        if(ownerBalance < amount){
            return #err("Insuficient balance");
        }
        else{
            return #ok(wallets.put(owner, ownerBalance - amount));
        }
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        if(caller != from){
            return #err("Unidentified Caller");
        };

        switch(wallets.get(from)){
            case(null){
                return #err("Unidentified Sender")
            };
            case(?sender){
                switch(wallets.get(to)){
                    case(null){
                        return #err("Unidentified Wallet Target")
                    };
                    case(?reciever){
                        wallets.put(from, sender - amount);
                        wallets.put(to, reciever + amount);
                        return #ok();
                    }
                }
            }
        }

    };

    public query func balanceOf(account : Principal) : async Nat {
        let accountBalance = Option.get(wallets.get(account), 0);
        return accountBalance;
    };

    public query func totalSupply() : async Nat {
        var sum = 0;
        for(v in wallets.vals()){
            sum += v;
        };
        return sum;
    };

};