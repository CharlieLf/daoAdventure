import Result "mo:base/Result";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import HashMap "mo:base/HashMap";
import Time "mo:base/Time";
import Option "mo:base/Option";
import Nat64 "mo:base/Nat64";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Types "types";

actor {

        type Result<A, B> = Result.Result<A, B>;
        type Member = Types.Member;
        type ProposalContent = Types.ProposalContent;
        type ProposalId = Types.ProposalId;
        type Proposal = Types.Proposal;
        type Vote = Types.Vote;
        type HttpRequest = Types.HttpRequest;
        type HttpResponse = Types.HttpResponse;

        // The principal of the Webpage canister associated with this DAO canister (needs to be updated with the ID of your Webpage canister)
        stable let canisterIdWebpage : Principal = Principal.fromText("4y4bj-oaaaa-aaaab-qac4q-cai");
        stable var manifesto = "Your manifesto";
        stable let name = "Your DAO";
        stable var goals : [Text] = [];
        
        public type TokenFaucet = actor {
                balanceOf : shared query Principal -> async Nat;
                balanceOfArray : shared query [Principal] -> async [Nat];
                burn : shared (Principal, Nat) -> async Result<(), Text>;
                mint : shared (Principal, Nat) -> async Result<(), Text>;
                tokenName : shared query () -> async Text;
                tokenSymbol : shared query () -> async Text;
                totalSupply : shared query () -> async Nat;
                transfer : shared (Principal, Principal, Nat) -> async Result<(), Text>;
        };
        let actorFaucet : TokenFaucet = actor("jaamb-mqaaa-aaaaj-qa3ka-cai");

        // Returns the name of the DAO
        public query func getName() : async Text {
                return name;
        };

        // Returns the manifesto of the DAO
        public query func getManifesto() : async Text {
                return manifesto;
        };

        // Returns the goals of the DAO
        public query func getGoals() : async [Text] {
                return goals;
        };

        // Register a new member in the DAO with the given name and principal of the caller
        // Airdrop 10 MBC tokens to the new member
        // New members are always Student
        // Returns an error if the member already exists
        let members = HashMap.HashMap<Principal, Member>(0, Principal.equal, Principal.hash);
        let initialMember : Member = {
                name = "motoko_bootcamp";
                role = #Mentor;
        };
        members.put(Principal.fromText("nkqop-siaaa-aaaaj-qa3qq-cai"), initialMember);

        public shared ({ caller }) func registerMember(member : Member) : async Result<(), Text> {
                switch(members.get(caller)){
                        case (null){
                                members.put(caller, member);
                                switch(await actorFaucet.mint(caller, 10)){
                                        case(#err(msg)){
                                                return #err("Fail add Token");
                                        };
                                        case(#ok()){};
                                };
                                return #ok();
                        };
                        case(?member){
                                return #err("Member already exists");
                        };
                };
        };

        // Get the member with the given principal
        // Returns an error if the member does not exist
        public query func getMember(p : Principal) : async Result<Member, Text> {
                switch(members.get(p)){
                        case(null){
                                return #err("Member not exist");
                        };
                        case(?member){
                                return #ok(member);
                        }
                }
        };

        // Graduate the student with the given principal
        // Returns an error if the student does not exist or is not a student
        // Returns an error if the caller is not a mentor
        public shared ({ caller }) func graduate(student : Principal) : async Result<(), Text> {
                let s = members.get(student);
                if(s == null){
                        return #err("Student not exist");
                };

                let studentMember = switch s {
                        case (?m) m;
                        case (null) return #err("Test");
                };

                if(studentMember.role != #Student){
                        return #err("Cant Graduate the role of : ");
                };

                let mentor = members.get(caller);
                if(studentMember.role != #Mentor){
                        return #err("Only mentor can graduate student");
                };

                let newGraduate : Member = {
                        name = studentMember.name;
                        role = #Graduate;
                };
                members.put(student, newGraduate);
                return #ok();
        };

        // Create a new proposal and returns its id
        // Returns an error if the caller is not a mentor or doesn't own at least 1 MBC token
        var proposalIncrementID : Nat = 0;
        let proposals = HashMap.HashMap<Nat64, Proposal>(0, Nat64.equal, Nat64.toNat32);
        public shared ({ caller }) func createProposal(content : ProposalContent) : async Result<ProposalId, Text> {
                let m = members.get(caller);
                let mentor = switch(m){
                        case (?v) v;
                        case (null) {return #err("Mentor Null")};
                };
                if(mentor.role != #Mentor){
                        return #err("Only mentor can create proposal");
                };
                let mentorBalance = await actorFaucet.balanceOf(caller);
                if(mentorBalance < 1){
                        return #err("Minimum Balance is 1");
                };

                let proposal : Proposal = {
                    id = proposalIncrementID;
                    content = content;
                    creator = caller;
                    created = Time.now();
                    executed = null;
                    votes = [];
                    voteScore = 0;
                    status = #Open;
                };
                switch(await actorFaucet.burn(caller, 1)){
                                        case(#err(msg)){
                                                return #err("Fail add Token");
                                        };
                                        case(#ok()){};
                                };
                proposalIncrementID += 1;
                proposals.put(Nat64.fromNat(proposal.id), proposal);
                return #ok(proposal.id);
        };

        // Get the proposal with the given id
        // Returns an error if the proposal does not exist
        public query func getProposal(id : ProposalId) : async Result<Proposal, Text> {
                switch(proposals.get(Nat64.fromNat(id))){
                        case(null){
                                return #err("Proposal not Found");
                        };
                        case(?p){
                                return #ok(p);
                        }
                };
        };

        // Returns all the proposals
        public query func getAllProposal() : async [Proposal] {
                return Iter.toArray(proposals.vals());
        };

        // Vote for the given proposal
        // Returns an error if the proposal does not exist or the member is not allowed to vote
        public shared ({ caller }) func voteProposal(proposalId : ProposalId, yesOrNo : Bool) : async Result<(), Text> {
                

                switch(proposals.get(Nat64.fromNat(proposalId))){
                        case(null){
                                return #err("Proposal not found");
                        };
                        case(?proposal){
                                switch(members.get(caller)){
                                        case(null){
                                                return #err("Member not Exist");
                                        };
                                        case(?member){
                                                if(member.role == #Student)return #err("Student cant Vote");
                                                if(_hasVoted(proposal, caller) == true)return #err("Member already voted");
                                                let newProposal = _newProposal(proposal, yesOrNo, caller, await actorFaucet.balanceOf(caller));
                                                proposals.put(Nat64.fromNat(proposalId), newProposal);

                                                return #ok();
                                        }
                                }
                        };
                }

        };

        private func _hasVoted(proposal: Proposal, principal : Principal) : Bool{
                for(voter in proposal.votes.vals()){
                        if(voter.member == principal)return true;
                };      
                return false;
        };

        private func _newProposal(oldProposal : Proposal, voice : Bool, voter : Principal, power : Nat) : Proposal{
                let multiplier = switch(voice){
                        case(true) {1};
                        case(false) {-1};
                };
                let voterUnr = members.get(voter);
                let voterMember = switch(voterUnr){
                        case (?s) s;
                        case (null) return oldProposal;
                };
                var votePower = power;

                if(voterMember.role == #Mentor)votePower *= 5;
                let newVoteScore = oldProposal.voteScore + (votePower * multiplier);
                let newStatus = _decideStatus(newVoteScore);
                if(newStatus == #Accepted){
                        let res = _executeProposal(oldProposal.content);
                };

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

        type ProposalStatus = Types.ProposalStatus;
        private func _decideStatus(score : Int) : ProposalStatus{
                if(score >= 100){
                        return #Accepted;
                }
                else if(score <= -100){
                        return #Rejected;
                }
                else return #Open;
        };

        private func _executeProposal(content : ProposalContent) : Result<(), Text>{
                switch(content){
                        case(#ChangeManifesto(v)){
                                manifesto := v;
                                return #ok();
                        };
                        case(#AddGoal(v)){
                                goals := Array.append<Text>(goals, [v]);
                                return #ok();
                        };
                        case(#AddMentor(principal)){
                                let oldMember = members.get(principal);
                                let fom = switch oldMember{
                                        case (?d) d;
                                        case null return #err("NotFound");
                                };
                                let newMember : Member = {
                                        name = fom.name;
                                        role = #Mentor;
                                };
                                members.put(principal, newMember);
                                return #ok();
                        };
                }
        };

        // Returns the Principal ID of the Webpage canister associated with this DAO canister
        public query func getIdWebpage() : async Principal {
                return canisterIdWebpage;
        };

};
