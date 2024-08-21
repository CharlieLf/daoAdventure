import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Principal "mo:base/Principal";
import Option "mo:base/Option";
import Time "mo:base/Time";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Hash "mo:base/Hash";
import Text "mo:base/Text";
import Int64 "mo:base/Int64";
import Bool "mo:base/Bool";
import Array "mo:base/Array";
import Types "types";
actor {
    // For this level we need to make use of the code implemented in the previous projects.
    // The voting system will make use of previous data structures and functions.
    /////////////////
    //   TYPES    //
    ///////////////
    type Member = Types.Member;
    type Result<Ok, Err> = Types.Result<Ok, Err>;
    type HashMap<K, V> = Types.HashMap<K, V>;
    type Proposal = Types.Proposal;
    type ProposalContent = Types.ProposalContent;
    type ProposalId = Types.ProposalId;
    type Vote = Types.Vote;

    /////////////////
    // PROJECT #1 //
    ///////////////
    let goals = Buffer.Buffer<Text>(0);
    let name = "Motoko Bootcamp";
    var manifesto = "Empower the next generation of builders and make the DAO-revolution a reality";

    public shared query func getName() : async Text {
        return name;
    };

    public shared query func getManifesto() : async Text {
        return manifesto;
    };

    public func setManifesto(newManifesto : Text) : async () {
        manifesto := newManifesto;
        return;
    };

    public func addGoal(newGoal : Text) : async () {
        goals.add(newGoal);
        return;
    };

    public shared query func getGoals() : async [Text] {
        Buffer.toArray(goals);
    };

    /////////////////
    // PROJECT #2 //
    ///////////////
    let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);

    public shared ({ caller }) func addMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                members.put(caller, member);
                return #ok();
            };
            case (?member) {
                return #err("Member already exists");
            };
        };
    };

    public shared ({ caller }) func updateMember(member : Member) : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.put(caller, member);
                return #ok();
            };
        };
    };

    public shared ({ caller }) func removeMember() : async Result<(), Text> {
        switch (members.get(caller)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                members.delete(caller);
                return #ok();
            };
        };
    };

    public query func getMember(p : Principal) : async Result<Member, Text> {
        switch (members.get(p)) {
            case (null) {
                return #err("Member does not exist");
            };
            case (?member) {
                return #ok(member);
            };
        };
    };

    public query func getAllMembers() : async [Member] {
        return Iter.toArray(members.vals());
    };

    public query func numberOfMembers() : async Nat {
        return members.size();
    };

    /////////////////
    // PROJECT #3 //
    ///////////////
    let ledger = HashMap.HashMap<Principal, Nat>(0, Principal.equal, Principal.hash);

    public query func tokenName() : async Text {
        return "Motoko Bootcamp Token";
    };

    public query func tokenSymbol() : async Text {
        return "MBT";
    };

    public func mint(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        ledger.put(owner, balance + amount);
        return #ok();
    };

    public func burn(owner : Principal, amount : Nat) : async Result<(), Text> {
        let balance = Option.get(ledger.get(owner), 0);
        if (balance < amount) {
            return #err("Insufficient balance to burn");
        };
        ledger.put(owner, balance - amount);
        return #ok();
    };

    func _burn(owner : Principal, amount : Nat) : () {
        let balance = Option.get(ledger.get(owner), 0);
        assert balance > amount;
        ledger.put(owner, balance - amount);
    };

    public shared ({ caller }) func transfer(from : Principal, to : Principal, amount : Nat) : async Result<(), Text> {
        let balanceFrom = Option.get(ledger.get(from), 0);
        let balanceTo = Option.get(ledger.get(to), 0);
        if (balanceFrom < amount) {
            return #err("Insufficient balance to transfer");
        };
        ledger.put(from, balanceFrom - amount);
        ledger.put(to, balanceTo + amount);
        return #ok();
    };

    public query func balanceOf(owner : Principal) : async Nat {
        return (Option.get(ledger.get(owner), 0));
    };

    public query func totalSupply() : async Nat {
        var total = 0;
        for (balance in ledger.vals()) {
            total += balance;
        };
        return total;
    };
    /////////////////
    // PROJECT #4 //
    ///////////////
    type ProposalStatus = Types.ProposalStatus;

    stable var indexProposalId : Nat64 = 0;
    let proposals = HashMap.HashMap<Nat64, Proposal>(0, Nat64.equal, Nat64.toNat32);
    public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
        if(Option.isNull(members.get(caller))){
            return #err("Caller not a member");
        };
        let memberBalance = Option.get(ledger.get(caller), 0);
        if(memberBalance < 1){
            return #err("Insufficient amount of balance");
        };

        switch(await getMember(caller)){
            case (#err(text)){
                return #err("Member not Registered");
            };
            case(#ok(member)){
                let proposal : Proposal = {
                    id = indexProposalId;
                    content = content;
                    creator = caller;
                    created = Time.now();
                    executed = null;
                    votes = [];
                    voteScore = 0;
                    status = #Open;
                };
                indexProposalId += 1;
                proposals.put(proposal.id, proposal);
                _burn(caller, 1);
                return #ok(proposal.id);
            };
        };
    };

    public query func getProposal(proposalId : ProposalId) : async ?Proposal {
        return proposals.get(proposalId);
    };

    public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
        if(Option.isNull(members.get(caller))){
            return #err("Caller not a member");
        };

        switch(await getProposal(proposalId)){
            case(null){
                return #err("Proposal Not Found");
            };
            case(?proposal){
                if(_hasVoted(proposal, caller))return #err("Member already Voted");
                let newProposal = _newProposal(proposal, yesOrNo, caller);
                proposals.put(proposalId, newProposal);

                return #ok();
                

            };
        };
    };

    public query func getAllProposals() : async [Proposal] {
        return Iter.toArray(proposals.vals());
    };

    private func _hasVoted(proposal: Proposal, principal : Principal) : Bool{
        for(voter in proposal.votes.vals()){
            if(voter.member == principal)return true;
        };
        return false;
    };

    private func _newProposal(oldProposal : Proposal, voice : Bool, voter : Principal) : Proposal{
        let multiplier = switch(voice){
            case(true) {1};
            case(false) {-1};
        };
        let votePower = Option.get(ledger.get(voter), 0);
        let newVoteScore = oldProposal.voteScore + (votePower * multiplier);
        let newStatus = _decideStatus(newVoteScore);
        let executedStatus : ?Time.Time = if(newStatus != #Open){
            ?Time.now();
        }
        else{
            null;
        };
        
        let newProposal : Proposal = {
            id = oldProposal.id;
            content = oldProposal.content;
            creator = oldProposal.creator;
            created = oldProposal.created;
            executed = executedStatus;
            votes = Array.append(oldProposal.votes, [_newVote(voter, voice, votePower)]);
            voteScore = newVoteScore;
            status = newStatus;
        };

        return newProposal;
    };

    private func _newVote(voter : Principal, voice : Bool, votePower : Nat) : Vote{
        let vote : Vote = {
            member = voter;
            votingPower = votePower;
            yesOrNo = voice;
        };
        return vote;
    };

    private func _decideStatus(score : Int) : ProposalStatus{
        if(score >= 100){
            return #Accepted;
        }
        else if(score <= -100){
            return #Rejected;
        }
        else return #Open;
    }
};