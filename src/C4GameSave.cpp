/*
 * LegacyClonk
 *
 * Copyright (c) RedWolf Design
 * Copyright (c) 2004, Sven2
 * Copyright (c) 2017-2019, The LegacyClonk Team and contributors
 *
 * Distributed under the terms of the ISC license; see accompanying file
 * "COPYING" for details.
 *
 * "Clonk" is a registered trademark of Matthes Bender, used with permission.
 * See accompanying file "TRADEMARK" for details.
 *
 * To redistribute this file separately, substitute the full license texts
 * for the above references.
 */

// game saving functionality

#include <C4Include.h>
#include <C4GameSave.h>
#include <C4Version.h>

#include <C4Components.h>
#include <C4Game.h>
#include <C4Console.h>
#include <C4Log.h>
#include <C4Player.h>

// *** C4GameSave main class

bool C4GameSave::SaveCreateGroup(const char *szFilename, CppC4Group &group)
{
	// erase any previous item (2do: work in C4Groups?)
	EraseItem(szFilename);
	// copy from previous group?
	if (GetCopyScenario())
		if (!ItemIdentical(Game.ScenarioFilename, szFilename))
			if (!CppC4Group_TransferItem(Game.ScenarioFilename, szFilename))
			{
				LogF(LoadResStr("IDS_CNS_SAVEASERROR"), szFilename); return false;
			}
	// open it
	if (!CppC4Group_Open(group, szFilename, !GetCopyScenario()))
	{
		EraseItem(szFilename);
		LogF(LoadResStr("IDS_CNS_SAVEASERROR"), szFilename);
		return false;
	}
	// done, success
	return true;
}

bool C4GameSave::SaveCore()
{
	// base on original, current core
	rC4S = Game.C4S;
	// Always mark current engine version
	rC4S.Head.C4XVer[0] = C4XVER1; rC4S.Head.C4XVer[1] = C4XVER2;
	rC4S.Head.C4XVer[2] = C4XVER3; rC4S.Head.C4XVer[3] = C4XVER4;
	// Some flags are not to be set for initial settings:
	//  They depend on whether specific runtime data is present, which may simply not be stored into initial
	//  saves, because they rely on any data present and up-to-date within the scenario!
	if (!fInitial)
	{
		// NoInitialize: Marks whether object data is contained and not to be created from core
		rC4S.Head.NoInitialize = true;
		// the SaveGame-value, despite it's name, marks whether exact runtime data is contained
		// the flag must not be altered for pure
		rC4S.Head.SaveGame = GetSaveRuntimeData() && IsExact();
	}
	// reset some network flags
	rC4S.Head.NetworkGame = 0;
	// Title in language game was started in (not: save scenarios and net references)
	if (!GetKeepTitle()) SCopy(Game.Parameters.ScenarioTitle.getData(), rC4S.Head.Title, C4MaxTitle);
	// some adjustments for everything but saved scenarios
	if (IsExact())
	{
		// Store used definitions
		rC4S.Definitions.SetModules(Game.DefinitionFilenames, Config.General.ExePath, Config.General.DefinitionPath);
		// Save game parameters
		if (!Game.Parameters.Save(*saveGroup, &Game.C4S)) return false;
	}
	// clear MissionAccess in save games and records (sulai)
	*rC4S.Head.MissionAccess = 0;
	// OldGfx is no longer supported
	// checks for IsExact() || ExactLandscape wouldn't catch scenarios using more than 23 materials, so let's make it easy
	rC4S.Head.ForcedGfxMode = C4SGFXMODE_NEWGFX;
	// store origin
	if (GetSaveOrigin())
	{
		// keep if assigned already (e.g., when doing a record of a savegame)
		if (!rC4S.Head.Origin.getLength())
		{
			rC4S.Head.Origin.Copy(Game.ScenarioFilename);
			Config.ForceRelativePath(&rC4S.Head.Origin);
		}
	}
	else if (GetClearOrigin())
		rC4S.Head.Origin.Clear();
	// adjust specific values (virtual call)
	AdjustCore(rC4S);
	// Save scenario core
	return !!rC4S.Save(*saveGroup);
}

bool C4GameSave::SaveScenarioSections()
{
	// any scenario sections?
	if (!Game.pScenarioSections) return true;
	// prepare section filename
	int iWildcardPos = SCharPos('*', C4CFN_ScenarioSections);
	char fn[_MAX_FNAME + 1];
	// save all modified sections
	for (C4ScenarioSection *pSect = Game.pScenarioSections; pSect; pSect = pSect->pNext)
	{
		// compose section filename
		SCopy(C4CFN_ScenarioSections, fn);
		SDelete(fn, 1, iWildcardPos); SInsert(fn, pSect->szName, iWildcardPos);
		// do not save self, because that is implied in CurrentScenarioSection and the main landscape/object data
		if (pSect == Game.pCurrentScenarioSection)
			saveGroup->deleteEntry(fn);
		else if (pSect->fModified)
		{
			// modified section: delete current
			saveGroup->deleteEntry(fn);
			// replace by new
			saveGroup->addFromDisk(pSect->szTempFilename, fn);
		}
	}
	// done, success
	return true;
}

bool C4GameSave::SaveLandscape()
{
	// exact?
	if (Game.Landscape.Mode == C4LSC_Exact || GetForceExactLandscape())
	{
		C4DebugRecOff DBGRECOFF;
		// Landscape
		Game.Objects.RemoveSolidMasks();
		bool fSuccess;
		if (Game.Landscape.Mode == C4LSC_Exact)
			fSuccess = !!Game.Landscape.Save(*saveGroup);
		else
			fSuccess = !!Game.Landscape.SaveDiff(*saveGroup, !IsSynced());
		Game.Objects.PutSolidMasks();
		if (!fSuccess) return false;
		DBGRECOFF.Clear();
		// PXS
		if (!Game.PXS.Save(*saveGroup)) return false;
		// MassMover (create copy, may not modify running data)
		C4MassMoverSet MassMoverSet;
		MassMoverSet.Copy(Game.MassMover);
		if (!MassMoverSet.Save(*saveGroup)) return false;
		// Material enumeration
		if (!Game.Material.SaveEnumeration(*saveGroup)) return false;
	}
	// static / dynamic
	if (Game.Landscape.Mode == C4LSC_Static)
	{
		// static map
		// remove old-style landscape.bmp
		saveGroup->deleteEntry(C4CFN_Landscape);
		// save materials if not already done
		if (!GetForceExactLandscape())
		{
			// save map
			if (!Game.Landscape.SaveMap(*saveGroup)) return false;
			// save textures (if changed)
			if (!Game.Landscape.SaveTextures(*saveGroup)) return false;
		}
	}
	else if (Game.Landscape.Mode != C4LSC_Exact)
	{
		// dynamic map by landscape.txt or scenario core: nothing to save
		// in fact, it doesn't even make much sense to save the Objects.txt
		// but the user pressed save after all...
	}
	return true;
}

bool C4GameSave::SaveRuntimeData()
{
	// scenario sections (exact only)
	if (IsExact()) if (!SaveScenarioSections())
	{
		Log(LoadResStr("IDS_ERR_SAVE_SCENSECTIONS")); return false;
	}
	// landscape
	if (!SaveLandscape()) { Log(LoadResStr("IDS_ERR_SAVE_LANDSCAPE")); return false; }
	// Strings
	Game.ScriptEngine.Strings.EnumStrings();
	if (!Game.ScriptEngine.Strings.Save(*saveGroup))
	{
		Log(LoadResStr("IDS_ERR_SAVE_SCRIPTSTRINGS")); return false;
	}
	// Objects
	if (!Game.Objects.Save(*saveGroup, IsExact(), true))
	{
		Log(LoadResStr("IDS_ERR_SAVE_OBJECTS")); return false;
	}
	// Round results
	if (GetSaveUserPlayers()) if (!Game.RoundResults.Save(*saveGroup))
	{
		Log(LoadResStr("IDS_ERR_ERRORSAVINGROUNDRESULTS")); return false;
	}
	// Teams
	if (!Game.Teams.Save(*saveGroup))
	{
		Log(LoadResStr(LoadResStr("IDS_ERR_ERRORSAVINGTEAMS"))); return false;
	}
	// some scenario components possiby modified in console mode
	// such modifications cannot possibly be done before game start
	// so it's runtime data
	// Script
	if (!Game.Script.Save(*saveGroup)) Log(LoadResStr("IDS_ERR_SAVE_SCRIPT")); /* nofail */
	// Title - unexact only, because in savegames, the title will be set in core
	if (!IsExact()) if (!Game.Title.Save(*saveGroup)) Log(LoadResStr("IDS_ERR_SAVE_TITLE")); /* nofail */
	// Info
	if (!Game.Info.Save(*saveGroup)) Log(LoadResStr("IDS_ERR_SAVE_INFO")); /* nofail */
	if (GetSaveUserPlayers() || GetSaveScriptPlayers())
	{
		// player infos
		// the stored player info filenames will point into the scenario file, and no ressource information
		// will be saved. PlayerInfo must be saved first, because those will generate the storage filenames to be used by
		// C4PlayerList
		C4PlayerInfoList RestoreInfos;
		RestoreInfos.SetAsRestoreInfos(Game.PlayerInfos, GetSaveUserPlayers(), GetSaveScriptPlayers(), GetSaveUserPlayerFiles(), GetSaveScriptPlayerFiles());
		if (!RestoreInfos.Save(*saveGroup, C4CFN_SavePlayerInfos))
		{
			Log(LoadResStr("IDS_ERR_SAVE_RESTOREPLAYERINFOS")); return false;
		}
		// Players
		// this will save the player files to the savegame scenario group only
		// synchronization to the original player files will be done in global game
		// synchronization (via control queue)
		if (GetSaveUserPlayerFiles() || GetSaveScriptPlayerFiles())
		{
			if (!Game.Players.Save(*saveGroup, GetCreateSmallFile(), RestoreInfos))
			{
				Log(LoadResStr("IDS_ERR_SAVE_PLAYERS")); return false;
			}
		}
	}
	else
	{
		// non-exact runtime data: remove any exact files
		// No Game.txt
		saveGroup->deleteEntry(C4CFN_Game);
		// No player files
		saveGroup->deleteEntry(C4CFN_PlayerInfos);
		saveGroup->deleteEntry(C4CFN_SavePlayerInfos);
	}
	// done, success
	return true;
}

bool C4GameSave::SaveDesc(CppC4Group &group)
{
	// Unfortunately, there's no way to prealloc the buffer in an appropriate size
	StdStrBuf sBuffer;

	// Header
	sBuffer.AppendFormat("{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang1031{\\fonttbl {\\f0\\fnil\\fcharset%d Times New Roman;}}", GetCharsetCode(Config.General.LanguageCharset));
	sBuffer.Append(LineFeed);

	// Scenario title
	sBuffer.AppendFormat("\\uc1\\pard\\ulnone\\b\\f0\\fs20 %s\\par", Game.Parameters.ScenarioTitle.getData());
	sBuffer.Append(LineFeed "\\b0\\fs16\\par" LineFeed);

	// OK; each specializations has its own desc format
	WriteDesc(sBuffer);

	// End of file
	sBuffer.Append(LineFeed "}" LineFeed EndOfFile);

	// Generate Filename
	char szLang[3];
	SCopyUntil(Config.General.Language, szLang, ',', 2);
	std::string filename{FormatString(C4CFN_ScenarioDesc, szLang).getData()};

	// Save to file
	if (!group.getEntryInfo(filename))
	{
		group.createFile(filename);
	}

	size_t size = sBuffer.getLength();
	char *pointer = sBuffer.GrabPointer();
	if (!group.setEntryData(filename, pointer, size, CppC4Group::MemoryManagement::Take))
	{
		free(pointer);
		return false;
	}
	return true;
}

void C4GameSave::WriteDescLineFeed(StdStrBuf &sBuf)
{
	// paragraph end + cosmetics
	sBuf.Append("\\par" LineFeed);
}

void C4GameSave::WriteDescDate(StdStrBuf &sBuf, bool fRecord)
{
	// write local time/date
	time_t tTime; time(&tTime);
	struct tm *pLocalTime;
	pLocalTime = localtime(&tTime);
	sBuf.AppendFormat(LoadResStr(fRecord ? "IDS_DESC_DATEREC" : (Game.Network.isEnabled() ? "IDS_DESC_DATENET" : "IDS_DESC_DATE")),
		pLocalTime->tm_mday,
		pLocalTime->tm_mon + 1,
		pLocalTime->tm_year + 1900,
		pLocalTime->tm_hour,
		pLocalTime->tm_min);
	WriteDescLineFeed(sBuf);
}

void C4GameSave::WriteDescGameTime(StdStrBuf &sBuf)
{
	// Write game duration
	if (Game.Time)
	{
		sBuf.AppendFormat(LoadResStr("IDS_DESC_DURATION"),
			Game.Time / 3600, (Game.Time % 3600) / 60, Game.Time % 60);
		WriteDescLineFeed(sBuf);
	}
}

void C4GameSave::WriteDescEngine(StdStrBuf &sBuf)
{
	char ver[5]; sprintf(ver, "%03d", (int)C4XVERBUILD);
	sBuf.AppendFormat(LoadResStr("IDS_DESC_VERSION"), ver);
	WriteDescLineFeed(sBuf);
}

void C4GameSave::WriteDescLeague(StdStrBuf &sBuf, bool fLeague, const char *strLeagueName)
{
	if (fLeague)
	{
		sBuf.AppendFormat(LoadResStr("IDS_PRC_LEAGUE"), strLeagueName);
		WriteDescLineFeed(sBuf);
	}
}

void C4GameSave::WriteDescDefinitions(StdStrBuf &sBuf)
{
	// Definition specs
	if (Game.DefinitionFilenames.size())
	{
		// Desc
		sBuf.Append(LoadResStr("IDS_DESC_DEFSPECS"));
		// Get definition modules
		int32_t i = 0;
		for (const auto &def : Game.DefinitionFilenames)
		{
			// Get exe relative path
			StdStrBuf sDefFilename;
			sDefFilename.Copy(Config.AtExeRelativePath(def.c_str()));
			// Convert rtf backslashes
			sDefFilename.Replace("\\", "\\\\");
			// Append comma
			if (i++ > 0) sBuf.Append(", ");
			// Apend to desc
			sBuf.Append(sDefFilename);
		}
		// End of line
		WriteDescLineFeed(sBuf);
	}
}

void C4GameSave::WriteDescNetworkClients(StdStrBuf &sBuf)
{
	// Desc
	sBuf.Append(LoadResStr("IDS_DESC_CLIENTS"));
	// Client names
	for (C4Network2Client *pClient = Game.Network.Clients.GetNextClient(nullptr); pClient; pClient = Game.Network.Clients.GetNextClient(pClient))
	{
		sBuf.Append(", "); sBuf.Append(pClient->getName());
	}
	// End of line
	WriteDescLineFeed(sBuf);
}

void C4GameSave::WriteDescPlayers(StdStrBuf &sBuf, bool fByTeam, int32_t idTeam)
{
	// write out all players; only if they match the given team if specified
	C4PlayerInfo *pPlr; bool fAnyPlrWritten = false;
	for (int i = 0; pPlr = Game.PlayerInfos.GetPlayerInfoByIndex(i); i++)
		if (pPlr->HasJoined() && !pPlr->IsRemoved() && !pPlr->IsInvisible())
		{
			if (fByTeam)
				if (idTeam)
				{
					// match team
					if (pPlr->GetTeam() != idTeam) continue;
				}
				else
				{
					// must be in no known team
					if (Game.Teams.GetTeamByID(pPlr->GetTeam())) continue;
				}
			if (fAnyPlrWritten)
				sBuf.Append(", ");
			else if (fByTeam && idTeam)
			{
				C4Team *pTeam = Game.Teams.GetTeamByID(idTeam);
				if (pTeam) sBuf.AppendFormat("%s: ", pTeam->GetName());
			}
			sBuf.Append(pPlr->GetName());
			fAnyPlrWritten = true;
		}
	if (fAnyPlrWritten) WriteDescLineFeed(sBuf);
}

void C4GameSave::WriteDescPlayers(StdStrBuf &sBuf)
{
	// New style using Game.PlayerInfos
	if (Game.PlayerInfos.GetPlayerCount())
	{
		sBuf.Append(LoadResStr("IDS_DESC_PLRS"));
		if (Game.Teams.IsMultiTeams() && !Game.Teams.IsAutoGenerateTeams())
		{
			// Teams defined: Print players sorted by teams
			WriteDescLineFeed(sBuf);
			C4Team *pTeam; int32_t i = 0;
			while (pTeam = Game.Teams.GetTeamByIndex(i++))
			{
				WriteDescPlayers(sBuf, true, pTeam->GetID());
			}
			// Finally, print out players outside known teams (those can only be achieved by script using SetPlayerTeam)
			WriteDescPlayers(sBuf, true, 0);
		}
		else
		{
			// No teams defined: Print all players that have ever joined
			WriteDescPlayers(sBuf, false, 0);
		}
	}
}

bool C4GameSave::Save(const char *szFilename)
{
	// close any previous
	Close();
	fileName = szFilename;
	// create group
	auto *group = new CppC4Group;
	if (!SaveCreateGroup(szFilename, *group))
	{
		LogF(LoadResStr("IDS_ERR_SAVE_TARGETGRP"), szFilename ? szFilename : "nullptr!");
		return false;
	}
	// save to it
	return Save(*group, true);
}

bool C4GameSave::Save(CppC4Group &group, bool fKeepGroup)
{
	// close any previous
	Close();
	// set group
	saveGroup = &group; fOwnGroup = fKeepGroup;
	// PreSave-actions (virtual call)
	if (!OnSaving()) return false;
	// always save core
	if (!SaveCore()) { Log(LoadResStr("IDS_ERR_SAVE_CORE")); return false; }
	// cleanup group
	CppC4Group_ForEachEntryByWildcard(group, "", C4CFN_PlayerFiles, [&group](const auto &info)
	{
		group.deleteEntry(info.fileName);
		return true;
	});
	// remove: Title text, image and icon if specified
	if (!GetKeepTitle())
	{
		saveGroup->deleteEntry(C4CFN_ScenarioTitle);
		saveGroup->deleteEntry(C4CFN_ScenarioIcon);
		CppC4Group_ForEachEntryByWildcard(*saveGroup, "", FormatString(C4CFN_ScenarioDesc, "*").getData(), [&group](const auto &info)
		{
			group.deleteEntry(info.fileName);
			return true;
		});
		saveGroup->deleteEntry(C4CFN_Titles);
		saveGroup->deleteEntry(C4CFN_Info);
	}
	// Always save Game.txt; even for saved scenarios, because global effects need to be saved
	if (!Game.SaveData(*saveGroup, false, fInitial, IsExact()))
	{
		Log(LoadResStr("IDS_ERR_SAVE_RUNTIMEDATA")); return false;
	}
	// save additional runtime data
	if (GetSaveRuntimeData()) if (!SaveRuntimeData()) return false;
	// Desc
	if (GetSaveDesc())
		if (!SaveDesc(*saveGroup))
			Log(LoadResStr("IDS_ERR_SAVE_DESC")); /* nofail */
	// save specialized components (virtual call)
	if (!SaveComponents()) return false;
	// done, success
	return true;
}

bool C4GameSave::Close()
{
	bool fSuccess = true;
	// any group open?
	if (saveGroup)
	{
		// close if owned group
		if (fOwnGroup)
		{
			saveGroup->save();
			delete saveGroup;
			fOwnGroup = false;
		}
		saveGroup = nullptr;
	}
	return fSuccess;
}

// *** C4GameSaveSavegame

bool C4GameSaveSavegame::OnSaving()
{
	if (!Game.IsRunning) return true;
	// synchronization to sync player files on all clients
	// this resets playing times and stores them in the players?
	// but doing so would be too late when the queue is executed!
	// TODO: remove it? (-> PeterW ;))
	if (Game.Network.isEnabled())
		Game.Input.Add(CID_Synchronize, new C4ControlSynchronize(true));
	else
		Game.Players.SynchronizeLocalFiles();
	// OK; save now
	return true;
}

void C4GameSaveSavegame::AdjustCore(C4Scenario &rC4S)
{
	// Determine save game index from trailing number in group file name
	int iSaveGameIndex = GetTrailingNumber(GetFilenameOnly(fileName.c_str()));
	// Looks like a decent index: set numbered icon
	if (Inside(iSaveGameIndex, 1, 10))
		rC4S.Head.Icon = 2 + (iSaveGameIndex - 1);
	// Else: set normal script icon
	else
		rC4S.Head.Icon = 29;
}

bool C4GameSaveSavegame::SaveComponents()
{
	// special for savegames: save a screenshot
	if (!Game.SaveGameTitle(*saveGroup))
		Log(LoadResStr("IDS_ERR_SAVE_GAMETITLE")); /* nofail */
	// done, success
	return true;
}

bool C4GameSaveSavegame::WriteDesc(StdStrBuf &sBuf)
{
	// compose savegame desc
	WriteDescDate(sBuf);
	WriteDescGameTime(sBuf);
	WriteDescDefinitions(sBuf);
	if (Game.Network.isEnabled()) WriteDescNetworkClients(sBuf);
	WriteDescPlayers(sBuf);
	// done, success
	return true;
}

// *** C4GameSaveRecord

void C4GameSaveRecord::AdjustCore(C4Scenario &rC4S)
{
	// specific recording flags
	rC4S.Head.Replay = true;
	rC4S.Head.Icon = 29;
	// default record title
	char buf[1024 + 1];
	sprintf(buf, "%03i %s [%d]", iNum, Game.Parameters.ScenarioTitle.getData(), (int)C4XVERBUILD);
	SCopy(buf, rC4S.Head.Title, C4MaxTitle);
}

bool C4GameSaveRecord::SaveComponents()
{
	// special: records need player infos even if done initially
	if (fInitial) Game.PlayerInfos.Save(*saveGroup, C4CFN_PlayerInfos);
	// for !fInitial, player infos will be saved as regular runtime data
	// done, success
	return true;
}

bool C4GameSaveRecord::WriteDesc(StdStrBuf &sBuf)
{
	// compose record desc
	WriteDescDate(sBuf, true);
	WriteDescGameTime(sBuf);
	WriteDescEngine(sBuf);
	WriteDescDefinitions(sBuf);
	WriteDescLeague(sBuf, fLeague, Game.Parameters.League.getData());
	if (Game.Network.isEnabled()) WriteDescNetworkClients(sBuf);
	WriteDescPlayers(sBuf);
	// done, success
	return true;
}

// *** C4GameSaveNetwork

void C4GameSaveNetwork::AdjustCore(C4Scenario &rC4S)
{
	// specific dynamic flags
	rC4S.Head.NetworkGame = true;
	rC4S.Head.NetworkRuntimeJoin = !fInitial;
}
