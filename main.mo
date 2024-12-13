import Result "mo:base/Result";
import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Array "mo:base/Array";
import Int "mo:base/Int";
import Buffer "mo:base/Buffer";
import Time "mo:base/Time";
import Float "mo:base/Float"

actor {
    // User Profile Types
    type UserId = Text;
    type UserProfile = {
        id: UserId;
        name: Text;
        contactInfo: Text;
        skills: [Text];
        organizationType: ?Text; // Optional organization affiliation
    };

    // Emergency Help Request Types
    type HelpRequestId = Text;
    type HelpRequestStatus = {
        #Pending;
        #InProgress;
        #Completed;
        #Urgent;
    };
    type HelpRequest = {
        id: HelpRequestId;
        requesterUserId: UserId;
        location: {
            latitude: Float;
            longitude: Float;
        };
        description: Text;
        resourcesNeeded: [Text];
        status: HelpRequestStatus;
        assignedVolunteers: [UserId];
        timestamp: Int;
    };

    // Task Tracking Type
    type TaskId = Text;
    type Task = {
        id: TaskId;
        helpRequestId: HelpRequestId;
        volunteerUserId: UserId;
        description: Text;
        status: HelpRequestStatus;
        startTime: Int;
        completionTime: ?Int;
    };

    // Storage for Platform Data
    let users = HashMap.HashMap<UserId, UserProfile>(10, Text.equal, Text.hash);
    let helpRequests = HashMap.HashMap<HelpRequestId, HelpRequest>(10, Text.equal, Text.hash);
    let tasks = HashMap.HashMap<TaskId, Task>(10, Text.equal, Text.hash);

    // User Profile Management
    public func registerUser(profile: UserProfile) : async Result.Result<UserId, Text> {
        if (profile.name == "") {
            return #err("Name cannot be empty");
        };
        
        users.put(profile.id, profile);
        #ok(profile.id)
    };

    public query func getUserProfile(userId: UserId) : async ?UserProfile {
        users.get(userId)
    };

    // Help Request Management
    public func createHelpRequest(request: HelpRequest) : async Result.Result<HelpRequestId, Text> {
        if (request.description == "") {
            return #err("Description cannot be empty");
        };
        
        helpRequests.put(request.id, request);
        #ok(request.id)
    };

    public func updateHelpRequestStatus(
        requestId: HelpRequestId, 
        newStatus: HelpRequestStatus
    ) : async Result.Result<(), Text> {
        switch (helpRequests.get(requestId)) {
            case (null) { #err("Help request not found") };
            case (?existingRequest) {
                let updatedRequest = {
                    existingRequest with 
                    status = newStatus
                };
                helpRequests.put(requestId, updatedRequest);
                #ok()
            };
        }
    };

    // Volunteer Task Management
    public func assignVolunteerToTask(
        helpRequestId: HelpRequestId, 
        volunteerUserId: UserId
    ) : async Result.Result<TaskId, Text> {
        switch (helpRequests.get(helpRequestId)) {
            case (null) { #err("Help request not found") };
            case (?helpRequest) {
                // Check if volunteer is already assigned
                // let alreadyAssigned = Array.find(
                //     helpRequest.assignedVolunteers, 
                //     func(id: UserId) { id == volunteerUserId }
                // );
                
                // if (Option.isSome(alreadyAssigned)) {
                //     return #err("Volunteer already assigned");
                // };

                // Create a new task
                let taskId = helpRequestId # "-" # volunteerUserId;
                let newTask : Task = {
                    id = taskId;
                    helpRequestId = helpRequestId;
                    volunteerUserId = volunteerUserId;
                    description = helpRequest.description;
                    status = #Pending;
                    startTime = Time.now();
                    completionTime = null;
                };

                // Update help request with new volunteer
                let updatedHelpRequest = {
                    helpRequest with 
                    assignedVolunteers = 
                        Array.append(helpRequest.assignedVolunteers, [volunteerUserId])
                };
                
                helpRequests.put(helpRequestId, updatedHelpRequest);
                tasks.put(taskId, newTask);

                #ok(taskId)
            };
        }
    };

    // Query Nearby Help Requests
    // public query func getNearbyHelpRequests(
    //     userLocation: {latitude: Float; longitude: Float}, 
    //     maxDistance: Float
    // ) : async [HelpRequest] {
    //     Array.filter(
    //         Buffer.toArray(Buffer.fromArray(helpRequests.vals())),
    //         func(request: HelpRequest) : Bool {
    //             let distance = calculateDistance(
    //                 userLocation.latitude, 
    //                 userLocation.longitude, 
    //                 request.location.latitude, 
    //                 request.location.longitude
    //             );
    //             distance <= maxDistance
    //         }
    //     )
    // };

   // Utility function to calculate distance between two geographical points
    func calculateDistance(
        lat1: Float, lon1: Float, 
        lat2: Float, lon2: Float
    ) : Float {
        let earthRadius : Float = 6371.0; // Kilometers
        let dLat = (lat2 - lat1) * Float.pi / 180.0;
        let dLon = (lon2 - lon1) * Float.pi / 180.0;
        let a = 
            Float.sin(dLat/2) * Float.sin(dLat/2) +
            Float.cos(lat1 * Float.pi / 180.0) * 
            Float.cos(lat2 * Float.pi / 180.0) * 
            Float.sin(dLon/2) * Float.sin(dLon/2);
        let c = 2.0 * Float.arctan2(Float.sqrt(a), Float.sqrt(1.0-a));
        earthRadius * c
    };
}
