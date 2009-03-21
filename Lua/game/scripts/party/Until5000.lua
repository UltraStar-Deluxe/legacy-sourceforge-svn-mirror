function InitPartyMode()
  -- PartyModi created by users should use tables
  Party.Mode.Name = {English = "Until 5000",
                     French  = "A 5000",
                     German  = "Bis 5000"}

  -- PartyModi delivered with USDX should use translateable Strings
  Party.Mode.Name = "PLUGIN_UNTIL5000_NAME"
  Party.Mode.Description = "PLUGIN_UNTIL5000_DESC"

  Party.Mode.Author = "Whiteshark & Hawkear"
  Party.Mode.Homepage = "http://ultrastardx.sourceforge.net/"
  Party.Mode.Version = "0.9"
  
  Party.ShowRateBar = true
  Party.ShowScore = true
  Party.ShowNotes = true
  Party.NoDuet = true -- dont sing duets - this would be unfair

  -- all the other settings are at default or loaded from ini
  
  return true
end

function Draw()
  local i
  for i=0,Party.Teams-1 do
    Party.Team[i].Bar = math.floor(Party.Team[i].Score / 50) 
    Party.Team[i].Percentage = Party.Team[i].Bar 
    if Party.Team[i].Score >= 5000 then
      return false -- end the round
    end
  end  
  return true -- continue with the round
end

function Finish()
  local i
  local winners={}
  for i=0,Party.Teams-1 do
    if Party.Team[i].Score >= 5000 then
      table.insert(winners,i) 
    end 
  end
  return winners
end
