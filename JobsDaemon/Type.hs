-- | Types used to define the job queue.
module JobsDaemon.Type (
      JobId, Job (..), JobType (..) {- from Model -}
    , JobsQueue (..) , JobsChan (..), JobsDepends (..)
    ) where

import Prelude

import Control.Concurrent (MVar, Chan)
import qualified Data.Map as M

import Model (JobId, Job (..), JobType (..))

-- | Contains the queue of jobs which can be processed and the graph of
-- dependencies.
data JobsQueue = JobsQueue { jqChan :: JobsChan, jqDepends :: MVar JobsDepends }

-- | Contains a queue of background jobs which can be processed right now.
-- Each job is associated to an action.
newtype JobsChan = JobsChan (Chan (JobId, IO ()))

-- | Maps each uncompleted parent job to a list of its dependent jobs.
-- Each mapped job contains its id, its action and possibly the list of the
-- remaining other dependencies of the job.
-- When a job completes, the corresponding entry from the map is removed and
-- each mapped job is :
--      - added to the 'JobsChan' if the list of remaining dependencies of the
--      job is empty ;
--      - added to the first job of its remaining dependencies if the list is
--      not empty.
newtype JobsDepends = JobsDepends (M.Map JobId [(JobId, IO (), [JobId])])
