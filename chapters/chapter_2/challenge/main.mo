import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Iter "mo:base/Iter";
import Types "types";
actor {

    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;

    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                return #ok(members.put(caller, member));
            };
            case(? rm){
                return #err("Member already registered")
            }
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch(members.get(p)){
            case(null){
                return #err("Member not Registered : " # Principal.toText(p));
            };
            case(? member){
                return #ok(member);
            }
        }
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                return #err("Member not found");
            };
            case(? registeredMember){
                members.put(caller, member);
                return #ok()
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch(members.get(caller)){
            case(null){
                return #err("Member not found");
            };
            case(? registeredMember){
                members.delete(caller);
                return #ok()
            };
        };
    };

    public shared ({ caller }) func whoami() : async Principal {
        return caller;
    }

};