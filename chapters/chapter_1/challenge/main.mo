import Buffer "mo:base/Buffer";
actor chapter_1{

    let name : Text = "chapter_1";
    var manifesto : Text = "This is manifesto";
    var goals = Buffer.Buffer<Text>(10);

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
        return Buffer.toArray<Text>(goals);
    };
};