module FeaturesSpec where

import Truth as Th
import Feature
import FeaturesRaw as Fr
import Test.Hspec
import Test.QuickCheck
import Tests as T
import FeaturesRaw as Raw
import Control.Lens
import Text.Pandoc.Options
import qualified Text.Pandoc.Class as PC
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Data.Text (pack)

type Acc = ([Feature], Feature)

spec :: Spec
spec = do
  describe "Pandoc transformer" $ do
    it "should return a sample features list" $ do
      sample <- sampleFeatures Fr.sample
      (extractFeaturesFromPandoc sample) `shouldBe` expected
  where
    sampleFeatures :: String -> IO Pandoc
    sampleFeatures raw = fmap mapRes (PC.runIO $ readMarkdown def (pack raw))
    mapRes (Left _) = error "Could not read sample data"
    mapRes (Right d) = d
    expected = 
      Features {_features = [
        Feature {_featureName = "Nodes", _userStories = [
          UserStory  {_userStoryDesc = "As a Node Operator, I want to install software from binary artifacts or a Docker image", _criteria = [
            Criteria  {_criteriaName = "Install rnode using tarball", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#installing-and-running-on-debian-from-deb-package", _steps = []},
            Criteria  {_criteriaName = "Install rnode using docker", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#installing-and-running-on-docker", _steps = []}]},
          UserStory  {_userStoryDesc = "As a Node Operator, I want to run software on Linux, macOS, and in Docker", _criteria = [
            Criteria  {_criteriaName = "Run rnode on Linux", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#installing-and-running-on-debian-from-deb-package", _steps = []},
            Criteria  {_criteriaName = "Run rnode on macOS", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#installing-and-running-on-macos-via-homebrew", _steps = []},
            Criteria  {_criteriaName = "Run rnode on docker", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#installing-and-running-on-docker", _steps = []}]},
          UserStory  {_userStoryDesc = "As a dApp Developer, I want to interface with the Rholang interpreter and evaluate smart contracts independently from the blockchain", _criteria = [
            Criteria  {_criteriaName = "A contract being run using rnode eval", _testName = "test/test_eval.py::test_eval", _status = Missing, _href = "---", _steps = [
              Step "given that rnode is running",
              Step "user executes all contracts from /rholang/examples/*.rho using rnode eval",
              Step "program exists with 0 for each of the contract"]},
            Criteria  {_criteriaName = "A contract being run using rnode repl", _testName = "test/test_repl.py::test_repl", _status = Missing, _href = "---", _steps = [
              Step "given that rnode is running",
              Step "user executes all contracts from /rholang/examples/*.rho using rnode repl",
              Step "program exists with 0 for each of the contract"]},
            Criteria  {_criteriaName = "REPL detects invalid rholang", _testName = "test/test_repl.py::test_repl_detects_invalid_rholang", _status = Missing, _href = "---", _steps = [
              Step "given that rnode is running",
              Step "user executes rholang code that is \"foo\"",
              Step "program exists with 1 and prints out coop.rchain.rholang.interpreter.errorsTopLevelFreeVariablesNotAllowedError"]}]},
          UserStory  {_userStoryDesc = "As a Node Operator, I want to have a default configuration and the ability to customize the configuration on the command line", _criteria = [
            Criteria  {_criteriaName = "Configure rnode using rnode.toml", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#configuration-file", _steps = []},
            Criteria  {_criteriaName = "Configure rnode using command line flags", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#configuration-flags", _steps = []}]},
          UserStory  {_userStoryDesc = "As a Node Operator, I want to monitor the performance, resource consumption, and status of my node", _criteria = [
            Criteria  {_criteriaName = "Monitor resource consumption and status", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#monitor-resource-consumption", _steps = []},
            Criteria  {_criteriaName = "Monitor performance", _testName = "not available", _status = Missing, _href = "https://github.com/rchain/rchain/#monitor-performance", _steps = []}]},
          UserStory  {_userStoryDesc = "As a Node Operator, when I start rnode, I want to provide my validator key and wallet key", _criteria = [
            Criteria  {_criteriaName = "Start node with validator key and wallet key", _testName = "not available", _status = Missing, _href = "documentation: https://github.com/rchain/rchain/#starting-node-as-a-validator", _steps = []}]}]},
        Feature {_featureName = "Peer to Peer Network", _userStories = [
          UserStory  {_userStoryDesc = "As a Node operator, I want to be able to bootstrap to the network by connecting to any known node", _criteria = [
            Criteria  {_criteriaName = "connecting to existing node", _testName = "test/test_p2p.py::test_connecting_to_existing_node", _status = Missing, _href = "---", _steps = [
              Step "given that standalone is a running node in a standalone mode",
              Step "start new node with --bootstrap pointing to standalone",
              Step "node should succesfully start and connect to standalone via protocol handshake"]},
            Criteria  {_criteriaName = "connecting to non-existing node", _testName = "test/test_p2p.py::test_connecting_to_non_existing_node", _status = Missing, _href = "---", _steps = [
              Step "start new node with --bootstrap pointing to some non-existing address",
              Step "node should exit",
              Step "exit code should be 1",
              Step "node should log that bootstrap could not been found"]}]},
          UserStory  {_userStoryDesc = "As a Node operator, once connected via a bootstrap node, I want to discover and connect to peers", _criteria = [
            Criteria  {_criteriaName = "discover other nodes", _testName = "test/test_p2p.py::test_discover_other_nodes", _status = Missing, _href = "---", _steps = [
              Step "create a p2p network with 3 nodes nodaA, nodeB and nodeC",
              Step "join p2p network as described in \"As a Node operator, I want to be able to bootstrap to the network by connecting to any known node\" bootstaping from nodeA",
              Step "after a period of time new node discovers nodeB and nodeC",
              Step "after a period of time new node connects (via protocol handshake) with nodeB and nodeC"]}]},
          UserStory  {_userStoryDesc = "As a Node operator, I want to know how many peers I am connected to", _criteria = [
            Criteria  {_criteriaName = "number of protocol peers", _testName = "test/test_p2p.py::test_number_of_protocol_peers", _status = Missing, _href = "---", _steps = [
              Step "create a p2p network with 3 nodes nodaA, nodeB and nodeC",
              Step "access nodeA http endpoint under /info should print connected_peers = 2",
              Step "access nodeA http endpoint under /peers to list nodeB and nodeC in JSON format"]},
            Criteria  {_criteriaName = "number of protocol peers", _testName = "test/test_p2p.py::test_number_of_discovery_peers", _status = Missing, _href = "---", _steps = [
              Step "create a p2p network with 3 nodes nodaA, nodeB and nodeC",
              Step "access nodeA http endpoint under /info should print discovered_peers = 2",
              Step "access nodeA http endpoint under /discovered-peers to list nodeB and nodeC in JSON format"]}]}]},
        Feature {_featureName = "Network Launch", _userStories = [
          UserStory  {_userStoryDesc = "As a platform stakeholder, I want a Coop-governed, community-driven, and independently verifiable validation of the genesis block used to launch a network", _criteria = []},
          UserStory  {_userStoryDesc = "As a platform stakeholder, I want a Coop-goverend, community-driven, and independently verifiable successful genesis ceremony", _criteria = [
            Criteria  {_criteriaName = "A successful genesis ceremony", _testName = "test/test_genesis_ceremony.py::test_successful_genesis_ceremony", _status = Missing, _href = "---", _steps = [
              Step "ceremonyMaster is instatantied with flags --required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB.",
              Step "validatorA and validatorB joins p2p, both pointing to ceremonyMaster as bootstrap",
              Step "ceremonyMaster sends UnapprovedBlock to validatorA and validatorB",
              Step "validatorA and validatorB receives UnapprovedBlock",
              Step "validatorA and validatorB send back BlockApproval",
              Step "ceremonyMaster transitions to ApprovedBlockReceivedHandler",
              Step "ceremonyMaster sends ApprovedBlock to validatorA and validatorB",
              Step "validatorA and validatorB transition to ApprovedBlockReceivedHandler",
              Step "ceremonyMaster, validatorA and validatorB tip points to block (genesis) where it has no parent and Bonds holds validatorA and validatorB"]},
            Criteria  {_criteriaName = "A successful genesis ceremony with read-only nodes joining", _testName = "not available", _status = Missing, _href = "---", _steps = [
              Step "ceremonyMaster is instatantied with flags --required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB.",
              Step "validatorA and validatorB joins p2p, both pointing to ceremonyMaster as bootstrap",
              Step "readOnlyA(read-only) joins p2p",
              Step "ceremonyMaster sends UnapprovedBlock to validatorA and validatorB",
              Step "validatorA and validatorB receives UnapprovedBlock",
              Step "validatorA and validatorB send back BlockApproval",
              Step "ceremonyMaster transitions to ApprovedBlockReceivedHandler",
              Step "ceremonyMaster sends ApprovedBlock to validatorA and validatorB",
              Step "validatorA and validatorB transition to ApprovedBlockReceivedHandler",
              Step "ceremonyMaster, validatorA and validatorB tip points to block (genesis) where it has no parent and Bonds holds validatorA and validatorB",
              Step "readOnlyA **never** transitions to ApprovedBlockReceivedHandler"]},
            Criteria  {_criteriaName = "A NOT successful genesis ceremony (not enough sigs)", _testName = "not available", _status = Missing, _href = "---", _steps = [
              Step "ceremonyMaster is instatantied with flags --required-sigs 3 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB.",
              Step "validatorA and validatorB joins p2p, both pointing to ceremonyMaster as bootstrap",
              Step "ceremonyMaster sends UnapprovedBlock to validatorA and validatorB",
              Step "validatorA and validatorB receives UnapprovedBlock",
              Step "validatorA and validatorB send back BlockApproval",
              Step "ceremonyMaster logs an error about not getting enough signatures on time (duration)"]}]},
          UserStory  {_userStoryDesc = "As a Node operator, I want to join the network after genesis ceremony, receive the genesis block, and know the state of the blockchain", _criteria = [
            Criteria  {_criteriaName = "A validator catching up after genesis ceremony", _testName = "not available", _status = Missing, _href = "---", _steps = [
              Step "genesis reached as described in \"A successful genesis ceremony\"",
              Step "validatorC joins p2p, pointing on ceremonyMaster as bootstrap",
              Step "validatorC sends ApprovedBlockRequest to ceremonyMaster",
              Step "ceremonyMaster sends ApprovedBlock to validatorC",
              Step "validatorC transitions to ApprovedBlockReceivedHandler",
              Step "validatorC tip points to block (genesis) where it has no parent and Bonds holds validatorA and validatorB"]}]}]},
        Feature {_featureName = "Deployment", _userStories = [
          UserStory  {_userStoryDesc = "As a dApp developer I want to be able to deploy my rholang contract to a validator", _criteria = [
            Criteria  {_criteriaName = "A correct contract gets deployed successfully", _testName = "not available", _status = Missing, _href = "---", _steps = [
              Step "instantiate p2p network with single ceremonyMaster that transitions to ApprovedBlockReceivedhandler (--required-sig 0)",
              Step "call rnode deploy with rholang/examples/tut-philosophers.rho on ceremonyMaster",
              Step "assert a success on std out",
              Step "rnode deploy exit code should be 0"]},
            Criteria  {_criteriaName = "An incorrect contract does not get deployed", _testName = "not available", _status = Missing, _href = "---", _steps = [
              Step "instantiate p2p network with single ceremonyMaster that transitions to ApprovedBlockReceivedhandler (--required-sig 0)",
              Step "call rnode deploy with invalid contract on ceremonyMaster",
              Step "assert a error logs on std out",
              Step "rnode deploy exit code should be 1"]}]},
          UserStory  {_userStoryDesc = "As a user I want to be able to store data using a rholang contract in the tuplespace.", _criteria = [
            Criteria  {_criteriaName = "A contract pointing to some data gets deployed, the data gets fetched and asserted.", _testName = "test/test_storage.py::test_data_is_stored_and_served_by_node", _status = Missing, _href = "---", _steps = [
              Step "instantiate p2p network with single ceremonyMaster that transitions to ApprovedBlockReceivedhandler (--required-sig 0)",
              Step "call rnode deploy & rnode propose with integration-tests/features/contracts/storage/store-data.rho on ceremonyMaster",
              Step "assert success on std out",
              Step "call rnode deploy & rnode propose with integration-tests/features/contracts/storage/read-data.rho on ceremonyMaster",
              Step "assert success on std out",
              Step "compare data sent and restored"]}]},
          UserStory  {_userStoryDesc = "As a user I want to know how much REV my deployment will cost", _criteria = []}]},
        Feature {_featureName = "REV", _userStories = [
          UserStory  {_userStoryDesc = "As a platform stakeholder, I want REV to be the currency token for the RChain platform", _criteria = []},
          UserStory  {_userStoryDesc = "As a REV holder, assuming I maintain control of my keys and properly use the wallet where I store REV, I expect my REV to never be lost", _criteria = []}]},
        Feature {_featureName = "Wallets", _userStories = [
          UserStory  {_userStoryDesc = "As a user, I want to be able to configure a coop-supplied wallet so that I can store REV in it", _criteria = []},
          UserStory  {_userStoryDesc = "As a user, I want to be able to interface with the coop-supplied wallet at the command line.", _criteria = []},
          UserStory  {_userStoryDesc = "As a user, I want to be able to add REV to my coop-supplied wallet so that I have available REV to pay for goods/services", _criteria = []},
          UserStory  {_userStoryDesc = "As a user, I want to be able to remove REV from my coop-supplied wallet so that I can pay for goods/services", _criteria = []},
          UserStory  {_userStoryDesc = "As a user, I want to be able to receive REV from another user by providing that user with the public key for my coop-supplied-wallet.", _criteria = []},
          UserStory  {_userStoryDesc = "As a user, I want to be able to send REV to the coop-supplied wallet of another user by specifying the public key to the coop-supplied wallet of that user.", _criteria = []},
          UserStory  {_userStoryDesc = "As a user of a coop-supplied wallet, I want to query a wallet contract (or the blocks) for my public address to get the history of all REV transfers to and/or from it", _criteria = []},
          UserStory  {_userStoryDesc = "As a recipient of REV (other than REV at mainnet launch 'genesis'), I can use a co-op supplied dApp to view my REV balance", _criteria = []},
          UserStory  {_userStoryDesc = "As a recipient of REV at mainnet launch 'genesis', I can use a co-op supplied wallet to view my REV balance after launch.", _criteria = []},
          UserStory  {_userStoryDesc = "As an organization holding REV, I need to have multiple approviesr for any REV transaction.", _criteria = []},
          UserStory  {_userStoryDesc = "As a validator, I can move Rev to/from the key-pair for one validator node to the key-pair for another validator node or that of the co-op supplied wallet dApp", _criteria = []},
          UserStory  {_userStoryDesc = "As a wallet dApp developer, I want to use Ethereum-style addresses for send transactions to specify the recipient, so that a) I can reuse available Ethereum utility libraries; b) so the QR code is smaller and thus faster to scan than it would be for a full public key; c) it is easier for users to verbally confirm their address versus public key; and d) so RChain is more palatable for the Ethereum community", _criteria = []},
          UserStory  {_userStoryDesc = "As a wallet dApp developer, I want to discover and connect to rNodes that support a particular version (release number and hash) and have a minimum number of active connections, so that user risks due to interoperability issues and sybil actors are minimized", _criteria = []},
          UserStory  {_userStoryDesc = "As a wallet user, I need a command line interface for interacting with wallets.", _criteria = []},
          UserStory  {_userStoryDesc = "As a dApp organization, I need to have multiple approvers for any send transaction.", _criteria = []}]},
        Feature {_featureName = "Validation", _userStories = [
          UserStory  {_userStoryDesc = "As a RChain validator, I want my validator identity to be different from the identity of my node and from the identity of my wallet", _criteria = [
            Criteria  {_criteriaName = "TBD", _testName = "not available", _status = Missing, _href = "TBD", _steps = []}]},
          UserStory  {_userStoryDesc = "As a RChain validator, I want to know when a block I propose is finalized and with what degree of confidence", _criteria = [
            Criteria  {_criteriaName = "TBD", _testName = "TBD", _status = Missing, _href = "---", _steps = [
              Step "run p2p network",
              Step "duplicate steps in CliqueOracleTest to get get a DAG of known shape",
              Step "run show-block on each block and assert known fault tolerance"]}]},
          UserStory  {_userStoryDesc = "As a RChain validator, I want to expose to the internet only those methods needed for production dApps", _criteria = []}]},
        Feature {_featureName = "Bonding/Unbonding", _userStories = [
          UserStory  {_userStoryDesc = "As a Node Validator, I want to be able to add my stake to the network and be recognized as a validator so I can participate in proof of stake consensus and be eligible to earn rewards (validating)", _criteria = [
            Criteria  {_criteriaName = "Bonding a validator", _testName = "test/test_heterogenous_validators.py::test_heterogenous_validators", _status = Missing, _href = "---", _steps = [
              Step "TBD"]}]},
          UserStory  {_userStoryDesc = "As a Node Validator, I want to be able to retrieve my stake from the network and no longer be recognized a as validator", _criteria = []},
          UserStory  {_userStoryDesc = "As an incoming Node Validator, I need confirmation of my request to bond", _criteria = []},
          UserStory  {_userStoryDesc = "As a platform stakeholder, I want to know only bonded validators are able to propose", _criteria = [
            Criteria  {_criteriaName = "TBD", _testName = "not available", _status = Missing, _href = "---", _steps = []}]}]},
        Feature {_featureName = "Consensus", _userStories = [
          UserStory  {_userStoryDesc = "As a Node Validator, who joins existing network, my node can catch up and finalize the same set of blocks that other nodes are finalizing", _criteria = [
            Criteria  {_criteriaName = "Catch up triggerd by next round from other validator", _testName = "TBD", _status = Missing, _href = "---", _steps = [
              Step "initiate p2p with 3 validators validatorA, validatorB, validatorC",
              Step "each validator runs 100 rounds of deploy and propose",
              Step "wait graceful period of 30 seconds",
              Step "validator validatorD joins the network",
              Step "validatorA runs deploy and propose",
              Step "wait graceful period of 30 seconds",
              Step "each validator should have a DAG with same set of finalized blocks"]},
            Criteria  {_criteriaName = "Catch up automatically", _testName = "TBD", _status = Missing, _href = "---", _steps = [
              Step "initiate p2p with 3 validators validatorA, validatorB, validatorC",
              Step "each validator runs 10 rounds of deploy and propose",
              Step "wait graceful period of 30 seconds",
              Step "validator validatorD joins the network",
              Step "wait graceful period of 10 seconds",
              Step "each validator should have a DAG with same set of finalized blocks"]}]},
          UserStory  {_userStoryDesc = "As a Node Validator I want consensus protocol to converge", _criteria = [
            Criteria  {_criteriaName = "5 validators deploying 200 blocks end up with the same DAG", _testName = "test/test_consensus.py::test_5val_200blocks", _status = Missing, _href = "---", _steps = [
              Step "initiate p2p with 5 validators validatorA, validatorB, validatorC, validatorD and validatorE",
              Step "each validator runs 200 rounds of deploy and propose",
              Step "wait graceful period of 30 seconds",
              Step "each validator should output exactly same DAG"]}]}]},
        Feature {_featureName = "Not_Grouped", _userStories = [
          UserStory  {_userStoryDesc = "COunt?", _criteria = [
            Criteria  {_criteriaName = "Count from show blocks", _testName = "test/test_internal.py::test_blocks_count_from_show_blocks", _status = Missing, _href = "---", _steps = [
              Step "TBD"]},
          Criteria  {_criteriaName = "Count from show blocks", _testName = "test/test_internal.py::test_extract_block_hash_from_propose_output", _status = Missing, _href = "---", _steps = [
              Step "TBD"]}]}]}]}